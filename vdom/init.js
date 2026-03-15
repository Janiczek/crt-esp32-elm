
const app = Elm.Main.init({
    node: document.getElementById('app'),
})

function complainNonFatal(message) {
    app.ports.onFailure.send(message);
    console.error(message);
}

function complainFatal(message) {
    alert(message);
    app.ports.onFailure.send(message);
    throw new Error(message);
}

if (!('serial' in navigator)) {
    complainFatal('Web Serial is not supported in this browser.');
}

let webSerialPort;
let writer;
let reader;

app.ports.connect.subscribe(async () => {
    try {
        // init Web Serial
        webSerialPort = await navigator.serial.requestPort();
        await webSerialPort.open({ baudRate: 115200 });
        writer = webSerialPort.writable.getWriter();
        reader = webSerialPort.readable.getReader();

        writer.write(new Uint8Array([0x00])); // 0x00 -> please send current ESP32 data

        const esp32Data = await readEsp32Data();
        const dataView = new DataView(esp32Data);
        app.ports.onConnectSuccessful.send(dataView);
    } catch (e) {
        complainNonFatal('Failed to connect: ' + e.message);
    }
})

app.ports.disconnect.subscribe(async () => {
    if (reader)        { try { await reader.cancel();       } catch (_) {} reader = null; }
    if (writer)        { try { await writer.close();        } catch (_) {} writer = null; }
    if (webSerialPort) { try { await webSerialPort.close(); } catch (_) {} webSerialPort = null; }
    app.ports.onDisconnectSuccessful.send();
})

app.ports.sendCommand.subscribe((bytes) => {
    if (writer) {
        writer.write(bytes);
    } else {
        complainNonFatal('Cannot send command: not connected')
    }
})

function parseUint16LE(bytes) {
    return bytes[0] | (bytes[1] << 8);
}

function byteStream(reader) {
    let buf = new Uint8Array(0);
    let offset = 0;

    async function ensure(n) {
        while (buf.length - offset < n) {
            const { value, done } = await reader.read();
            if (done) {
                if (buf.length - offset < n) complainFatal('Stream ended unexpectedly');
                return;
            }
            if (offset >= buf.length) {
                buf = value;
                offset = 0;
            } else {
                const keep = buf.subarray(offset);
                buf = new Uint8Array(keep.length + value.length);
                buf.set(keep);
                buf.set(value, keep.length);
                offset = 0;
            }
        }
    }

    return {
        async readByte() {
            await ensure(1);
            if (offset >= buf.length) return null;
            return buf[offset++];
        },
        async readBytes(n) {
            await ensure(n);
            const out = buf.slice(offset, offset + n);
            offset += n;
            return out;
        },
    };
}

const SEPARATOR = [0xff, 0x00, 0xff, 0x00];

async function readEsp32Data() {
    try {
        const stream = byteStream(reader);

        let matched = 0;
        while (matched < SEPARATOR.length) {
            const b = await stream.readByte();
            if (b === null) complainFatal('Stream ended before finding separator');
            if (b === SEPARATOR[matched]) matched++;
            else matched = b === SEPARATOR[0] ? 1 : 0;
        }

        const lengthBuf = await stream.readBytes(2);
        const dataLength = parseUint16LE(lengthBuf);

        const dataBuf = await stream.readBytes(dataLength);
        return dataBuf.buffer;
    } catch (e) {
        if (e.name !== 'NetworkError') {
            complainNonFatal('Failed to read ESP32 data: ' + e.message);
        }
    } finally {
        reader.releaseLock();
    }
}
