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

const ACK_SEQUENCE = [0xFF, 0xEE, 0xFF, 0xEE];

let commandQueue = [];
let isWaitingForAck = false;
let lastSentCommandTag = null;

function bytesToAsciiVisible(u8) {
    let out = '';
    for (let i = 0; i < u8.length; i++) {
        const b = u8[i];
        if (b === 0x0a) out += '\n';
        else if (b === 0x0d) out += '\r';
        else if (b === 0x09) out += '\t';
        else if (b === 0x00) out += '\\0';
        else if (b >= 0x20 && b <= 0x7e) out += String.fromCharCode(b);
        else out += '\\x' + (b < 16 ? '0' : '') + b.toString(16);
    }
    return out;
}

async function trySendNextCommand() {
    if (isWaitingForAck || commandQueue.length === 0 || !writer) return;
    const item = commandQueue.shift();
    try {
        console.log(`[Elm->C] Sending command (${item.bytes.length} bytes)`);
        await writer.write(item.bytes);
    } catch (e) {
        complainNonFatal('Failed to send command: ' + e.message);
        return;
    }
    if (item.needsAck) {
        isWaitingForAck = true;
    } else {
        trySendNextCommand();
    }
}

// Rate-limit adding new commands to the queue to ~60fps.
// We batch all commands received within a frame and enqueue them together.
let pendingCommands = [];
let flushPendingScheduled = false;
let flushPendingHandle = null;
const hasRAF = typeof requestAnimationFrame === 'function' && typeof cancelAnimationFrame === 'function';
const scheduleNextFrame = hasRAF ? requestAnimationFrame : (cb) => setTimeout(cb, 16);
const cancelNextFrame = hasRAF ? cancelAnimationFrame : (id) => clearTimeout(id);

function enqueueCommandAtMostOncePerFrame(bytesView, needsAck) {
    pendingCommands.push({ bytes: bytesView, needsAck });
    if (flushPendingScheduled) return;
    flushPendingScheduled = true;
    flushPendingHandle = scheduleNextFrame(() => {
        flushPendingScheduled = false;
        flushPendingHandle = null;
        if (pendingCommands.length === 0) return;
        for (let i = 0; i < pendingCommands.length; i++) {
            commandQueue.push(pendingCommands[i]);
        }
        pendingCommands = [];
        trySendNextCommand();
    });
}

async function startContinuousRead() {
    if (!webSerialPort || !webSerialPort.readable) {
        return;
    }

    // Acquire a reader dedicated to logging incoming data
    reader = webSerialPort.readable.getReader();

    (async () => {
        const TIMEOUT_MS = 20;
        let buffer = new Uint8Array(0);
        let flushTimeoutId = null;
        const decoder = new TextDecoder();
        let ackMatched = 0;

        const scanForAck = (chunk) => {
            for (let i = 0; i < chunk.length; i++) {
                if (chunk[i] === ACK_SEQUENCE[ackMatched]) {
                    ackMatched++;
                    if (ackMatched === ACK_SEQUENCE.length) {
                        ackMatched = 0;
                        isWaitingForAck = false;
                        const ackTag = lastSentCommandTag == null ? 0 : lastSentCommandTag;
                        lastSentCommandTag = null;
                        try {
                            app.ports.onCommandAck.send({ commandTag: ackTag });
                        } catch (_) {}
                    }
                } else {
                    ackMatched = chunk[i] === ACK_SEQUENCE[0] ? 1 : 0;
                }
            }
        };

        const scheduleFlush = () => {
            if (flushTimeoutId !== null) {
                clearTimeout(flushTimeoutId);
            }
            flushTimeoutId = setTimeout(() => {
                if (buffer.length > 0) {
                    const text = decoder.decode(buffer);
                    buffer = new Uint8Array(0);
                }
                flushTimeoutId = null;
            }, TIMEOUT_MS);
        };

        const appendToBuffer = (chunk) => {
            if (!chunk || chunk.length === 0) {
                return;
            }
            const combined = new Uint8Array(buffer.length + chunk.length);
            combined.set(buffer, 0);
            combined.set(chunk, buffer.length);
            buffer = combined;
        };

        try {
            while (true) {
                const { value, done } = await reader.read();
                if (done) {
                    break;
                }
                scanForAck(value);
                appendToBuffer(value);
                scheduleFlush();
            }
        } catch (e) {
            if (e.name !== 'NetworkError') {
                complainNonFatal('Failed during continuous serial read: ' + e.message);
            }
        } finally {
            if (flushTimeoutId !== null) {
                clearTimeout(flushTimeoutId);
                flushTimeoutId = null;
            }
            if (buffer.length > 0) {
                const text = decoder.decode(buffer);
            }
            try {
                reader.releaseLock();
            } catch (_) {}
            reader = null;
        }
    })();
}

app.ports.connect.subscribe(async () => {
    try {
        // init Web Serial
        webSerialPort = await navigator.serial.requestPort();
        await webSerialPort.open({ baudRate: 115200 });
        writer = webSerialPort.writable.getWriter();
        const initReader = webSerialPort.readable.getReader();

        writer.write(new Uint8Array([0x00])); // 0x00 -> please send current ESP32 data

        const esp32Data = await readEsp32Data(initReader);
        const dataView = new DataView(esp32Data);
        app.ports.onConnectSuccessful.send(dataView);

        // After initial handshake, start logging all subsequent data
        startContinuousRead();
    } catch (e) {
        complainNonFatal('Failed to connect: ' + e.message);
    }
})

app.ports.disconnect.subscribe(async () => {
    if (reader)        { try { await reader.cancel();       } catch (_) {} reader = null; }
    if (writer)        { try { await writer.close();        } catch (_) {} writer = null; }
    if (webSerialPort) { try { await webSerialPort.close(); } catch (_) {} webSerialPort = null; }
    if (flushPendingHandle !== null) {
        cancelNextFrame(flushPendingHandle);
        flushPendingHandle = null;
    }
    flushPendingScheduled = false;
    pendingCommands = [];
    commandQueue = [];
    isWaitingForAck = false;
    lastSentCommandTag = null;
    app.ports.onDisconnectSuccessful.send(null);
})

app.ports.sendCommand.subscribe(async ([commandBytes, needsAck]) => {
    if (!writer) {
        complainNonFatal('Cannot send command: not connected');
        return;
    }

    if (isWaitingForAck) {
        complainNonFatal('Cannot send command: waiting for ACK');
        return;
    }

    const bytes = new Uint8Array(commandBytes.buffer, commandBytes.byteOffset, commandBytes.byteLength);
    const commandTag = bytes.length > 0 ? bytes[0] : 0;

    try {
        await writer.write(bytes);
    } catch (e) {
        complainNonFatal('Failed to send command: ' + e.message);
        return;
    }

    if (needsAck) {
        isWaitingForAck = true;
        lastSentCommandTag = commandTag;
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

async function readEsp32Data(readerForInit) {
    try {
        const stream = byteStream(readerForInit);

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
        try {
            readerForInit.releaseLock();
        } catch (_) {}
    }
}
