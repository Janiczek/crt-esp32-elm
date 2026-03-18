const app = Elm.Main.init({
    node: document.getElementById('app'),
})

const SERIAL_BAUD_RATE = 115200
const SERIAL_BUFFER_SIZE = 65536
const PROTOCOL_VERSION = 1

const TRANSPORT_OPCODES = {
    BINARY: 0x2,
    CLOSE: 0x8,
    PING: 0x9,
    PONG: 0xA,
}

const FRAME_TYPES = {
    GET_ESP32_DATA: 0x01,
    ESP32_DATA_BEGIN: 0x02,
    SET_ROOT_NODE: 0x03,
    ACK: 0x04,
    LOG: 0x05,
    ERROR: 0x06,
    ESP32_DATA_CHUNK: 0x07,
    ESP32_DATA_END: 0x08,
}

const VALID_FRAME_TYPES = new Set(Object.values(FRAME_TYPES))
const VALID_TRANSPORT_START_BYTES = new Set([
    0x80 | TRANSPORT_OPCODES.BINARY,
    0x80 | TRANSPORT_OPCODES.CLOSE,
    0x80 | TRANSPORT_OPCODES.PING,
    0x80 | TRANSPORT_OPCODES.PONG,
])

const textDecoder = new TextDecoder()

function transportOpcodeName(opcode) {
    switch (opcode) {
        case TRANSPORT_OPCODES.BINARY:
            return 'BINARY'
        case TRANSPORT_OPCODES.CLOSE:
            return 'CLOSE'
        case TRANSPORT_OPCODES.PING:
            return 'PING'
        case TRANSPORT_OPCODES.PONG:
            return 'PONG'
        default:
            return `UNKNOWN(${opcode})`
    }
}

function frameTypeName(frameType) {
    for (const [name, value] of Object.entries(FRAME_TYPES)) {
        if (value === frameType) {
            return name
        }
    }
    return `UNKNOWN(${frameType})`
}

function debugSerial(message, details) {
    if (details === undefined) {
        console.log('[Serial]', message)
        return
    }
    console.log('[Serial]', message, details)
}

let serialPort = null
let serialReader = null
let serialWriter = null
let readLoopPromise = null
let transportReadBuffer = new Uint8Array(0)
let pendingLogText = ''
let isWaitingForAck = false
let explicitDisconnectRequested = false
let hasCompletedHandshake = false
let pendingCloseReason = null
let pendingEsp32Data = null

function complainNonFatal(message) {
    app.ports.onFailure.send(message);
    console.error(message);
}

function resetProtocolState() {
    debugSerial('[JS] resetting protocol state')
    isWaitingForAck = false
    hasCompletedHandshake = false
    pendingCloseReason = null
    pendingEsp32Data = null
    transportReadBuffer = new Uint8Array(0)
    pendingLogText = ''
}

function parseUint16Le(bytes, offset) {
    return bytes[offset] | (bytes[offset + 1] << 8)
}

function parseUint32Le(bytes, offset) {
    return (
        bytes[offset] |
        (bytes[offset + 1] << 8) |
        (bytes[offset + 2] << 16) |
        (bytes[offset + 3] << 24)
    ) >>> 0
}

function beginEsp32DataHandshake(payload) {
    if (hasCompletedHandshake) {
        throw new Error('Received duplicate ESP32 data handshake')
    }
    if (pendingEsp32Data !== null) {
        throw new Error('Received nested ESP32 data handshake')
    }
    if (payload.length !== 4 && payload.length !== 6) {
        throw new Error('Invalid ESP32 data begin payload')
    }

    const totalBytes = parseUint32Le(payload, 0)
    const expectedChunkCount = payload.length >= 6 ? parseUint16Le(payload, 4) : null

    pendingEsp32Data = {
        expectedTotalBytes: totalBytes,
        expectedChunkCount,
        chunks: [],
        receivedBytes: 0,
        receivedChunkCount: 0,
    }

    if (expectedChunkCount !== null) {
        app.ports.onInitialLoadChunkCount.send({
            expectedChunkCount,
            expectedTotalBytes: totalBytes,
        })
    }

    debugSerial('[JS reader] ESP32_DATA begin', {
        totalBytes,
        expectedChunkCount,
    })
}

function appendEsp32DataChunk(payload) {
    if (pendingEsp32Data === null) {
        throw new Error('Received ESP32 data chunk before begin')
    }

    pendingEsp32Data.receivedBytes += payload.length
    pendingEsp32Data.receivedChunkCount += 1

    if (pendingEsp32Data.receivedBytes > pendingEsp32Data.expectedTotalBytes) {
        throw new Error('Received too much ESP32 data')
    }

    pendingEsp32Data.chunks.push(payload.slice())
    app.ports.onInitialLoadChunkReceived.send(payload.length)

    debugSerial('[JS reader] ESP32_DATA chunk', {
        chunkBytes: payload.length,
        receivedBytes: pendingEsp32Data.receivedBytes,
        expectedTotalBytes: pendingEsp32Data.expectedTotalBytes,
        receivedChunkCount: pendingEsp32Data.receivedChunkCount,
        expectedChunkCount: pendingEsp32Data.expectedChunkCount,
    })
}

function finishEsp32DataHandshake() {
    if (pendingEsp32Data === null) {
        throw new Error('Received ESP32 data end before begin')
    }

    if (pendingEsp32Data.receivedBytes !== pendingEsp32Data.expectedTotalBytes) {
        throw new Error('Incomplete ESP32 data payload')
    }

    if (
        pendingEsp32Data.expectedChunkCount !== null &&
        pendingEsp32Data.receivedChunkCount !== pendingEsp32Data.expectedChunkCount
    ) {
        throw new Error('ESP32 data chunk count mismatch')
    }

    const payload = new Uint8Array(pendingEsp32Data.expectedTotalBytes)
    let offset = 0
    for (const chunk of pendingEsp32Data.chunks) {
        payload.set(chunk, offset)
        offset += chunk.length
    }

    pendingEsp32Data = null
    hasCompletedHandshake = true
    debugSerial('[JS reader] ESP32_DATA complete', {
        totalBytes: payload.length,
    })
    app.ports.onConnectSuccessful.send(new DataView(payload.buffer))
}

function makeFrame(frameType, payload) {
    const frame = new Uint8Array(2 + payload.length)
    frame[0] = frameType
    frame[1] = PROTOCOL_VERSION
    frame.set(payload, 2)
    return frame
}

function makeTransportFrame(opcode, payload) {
    if (payload.length > 0xffff) {
        throw new Error('Transport payload too large')
    }

    const header = payload.length <= 125 ? new Uint8Array(2) : new Uint8Array(4)
    header[0] = 0x80 | (opcode & 0x0f)
    if (payload.length <= 125) {
        header[1] = payload.length
    } else {
        header[1] = 126
        header[2] = (payload.length >> 8) & 0xff
        header[3] = payload.length & 0xff
    }

    const frame = new Uint8Array(header.length + payload.length)
    frame.set(header, 0)
    frame.set(payload, header.length)
    return frame
}

function appendBytes(left, right) {
    if (left.length === 0) {
        return right.slice()
    }
    if (right.length === 0) {
        return left.slice()
    }

    const combined = new Uint8Array(left.length + right.length)
    combined.set(left, 0)
    combined.set(right, left.length)
    return combined
}

function decodeText(payload) {
    return textDecoder.decode(payload);
}

function logNoiseBytes(bytes, forceFlush = false) {
    if (bytes.length > 0) {
        pendingLogText += textDecoder.decode(bytes, { stream: !forceFlush })
    }

    let newlineIndex = pendingLogText.indexOf('\n')
    while (newlineIndex >= 0) {
        const line = pendingLogText.slice(0, newlineIndex).replace(/\r$/, '').trim()
        if (line.length > 0) {
            console.log('[ESP32]', line)
        }
        pendingLogText = pendingLogText.slice(newlineIndex + 1)
        newlineIndex = pendingLogText.indexOf('\n')
    }

    if (forceFlush) {
        const tail = pendingLogText.trim()
        if (tail.length > 0) {
            console.log('[ESP32]', tail)
        }
        pendingLogText = ''
    }
}

function isKnownAppFrameType(frameType) {
    return VALID_FRAME_TYPES.has(frameType)
}

function isValidBinaryPayload(payload) {
    return payload.length >= 2 && payload[1] === PROTOCOL_VERSION && isKnownAppFrameType(payload[0])
}

function findTransportFrameStart(buffer) {
    for (let i = 0; i < buffer.length; i += 1) {
        if (VALID_TRANSPORT_START_BYTES.has(buffer[i])) {
            return i
        }
    }
    return -1
}

function tryParseTransportFrame(buffer) {
    if (buffer.length < 2) {
        return { status: 'need-more' }
    }

    const firstByte = buffer[0]
    if ((firstByte & 0x80) === 0) {
        return { status: 'invalid' }
    }

    const opcode = firstByte & 0x0f
    if (
        opcode !== TRANSPORT_OPCODES.BINARY &&
        opcode !== TRANSPORT_OPCODES.CLOSE &&
        opcode !== TRANSPORT_OPCODES.PING &&
        opcode !== TRANSPORT_OPCODES.PONG
    ) {
        return { status: 'invalid' }
    }

    const masked = (buffer[1] & 0x80) !== 0
    if (masked) {
        return { status: 'invalid' }
    }

    let headerLength = 2
    let payloadLength = buffer[1] & 0x7f

    if (payloadLength === 126) {
        if (buffer.length < 4) {
            return { status: 'need-more' }
        }
        headerLength = 4
        payloadLength = (buffer[2] << 8) | buffer[3]
    } else if (payloadLength === 127) {
        return { status: 'invalid' }
    }

    const totalLength = headerLength + payloadLength
    if (buffer.length < totalLength) {
        return { status: 'need-more' }
    }

    const payload = buffer.slice(headerLength, totalLength)
    if (opcode === TRANSPORT_OPCODES.BINARY && !isValidBinaryPayload(payload)) {
        return { status: 'invalid' }
    }

    return {
        status: 'ok',
        opcode,
        payload,
        totalLength,
    }
}

function closeAndReportFailure(message) {
    console.error('[Serial] closing transport after failure', message)
    pendingCloseReason = message
    if (serialPort) {
        void disconnectTransportInternal()
    } else {
        complainNonFatal(message);
        pendingCloseReason = null;
    }
}

function handleIncomingFrame(data) {
    const frame = data instanceof Uint8Array ? data : new Uint8Array(data)
    if (frame.length < 2) {
        throw new Error('Received short frame')
    }

    const frameType = frame[0];
    const version = frame[1];
    const payload = frame.subarray(2);

    debugSerial('[JS reader] incoming app frame', {
        frameType: frameTypeName(frameType),
        payloadBytes: payload.length,
    })

    if (version !== PROTOCOL_VERSION) {
        throw new Error(`Unsupported protocol version: ${version}`);
    }

    switch (frameType) {
        case FRAME_TYPES.ESP32_DATA_BEGIN: {
            beginEsp32DataHandshake(payload)
            break;
        }

        case FRAME_TYPES.ESP32_DATA_CHUNK: {
            appendEsp32DataChunk(payload)
            break;
        }

        case FRAME_TYPES.ESP32_DATA_END: {
            finishEsp32DataHandshake()
            break;
        }

        case FRAME_TYPES.ACK: {
            isWaitingForAck = false;
            debugSerial('[JS reader] received ACK')
            app.ports.onRootNodeAck.send(null);
            break;
        }

        case FRAME_TYPES.LOG:
            debugSerial('[JS reader] received LOG frame', { payloadBytes: payload.length })
            console.log('[ESP32]', decodeText(payload));
            break;

        case FRAME_TYPES.ERROR:
            console.error('[Serial] received ERROR frame', {
                payloadBytes: payload.length,
            })
            complainNonFatal(payload.length > 0 ? decodeText(payload) : 'ESP32 reported an error');
            break;

        default:
            throw new Error(`Unknown frame type: ${frameType}`);
    }
}

async function writeTransportFrame(opcode, payload) {
    if (!serialWriter) {
        throw new Error('Serial writer not available')
    }
    debugSerial('[JS writer] writing transport frame', {
        opcode: transportOpcodeName(opcode),
        payloadBytes: payload.length,
    })
    await serialWriter.write(makeTransportFrame(opcode, payload))
}

async function sendAppFrame(frameType, payload) {
    debugSerial('[JS writer] sending app frame', {
        frameType: frameTypeName(frameType),
        payloadBytes: payload.length,
    })
    await writeTransportFrame(TRANSPORT_OPCODES.BINARY, makeFrame(frameType, payload))
}

function processTransportReadBuffer() {
    for (;;) {
        if (transportReadBuffer.length === 0) {
            return
        }

        const frameStart = findTransportFrameStart(transportReadBuffer)
        if (frameStart < 0) {
            logNoiseBytes(transportReadBuffer)
            transportReadBuffer = new Uint8Array(0)
            return
        }

        if (frameStart > 0) {
            logNoiseBytes(transportReadBuffer.subarray(0, frameStart))
            transportReadBuffer = transportReadBuffer.slice(frameStart)
        }

        const parsed = tryParseTransportFrame(transportReadBuffer)
        if (parsed.status === 'need-more') {
            return
        }

        if (parsed.status === 'invalid') {
            logNoiseBytes(transportReadBuffer.subarray(0, 1))
            transportReadBuffer = transportReadBuffer.slice(1)
            continue
        }

        transportReadBuffer = transportReadBuffer.slice(parsed.totalLength)

        try {
            debugSerial('[JS reader] parsed transport frame', {
                opcode: transportOpcodeName(parsed.opcode),
                payloadBytes: parsed.payload.length,
                bufferedBytesRemaining: transportReadBuffer.length,
            })
            switch (parsed.opcode) {
                case TRANSPORT_OPCODES.BINARY:
                    handleIncomingFrame(parsed.payload)
                    break

                case TRANSPORT_OPCODES.CLOSE:
                    closeAndReportFailure('ESP32 closed the serial transport')
                    return

                case TRANSPORT_OPCODES.PING:
                    void writeTransportFrame(TRANSPORT_OPCODES.PONG, parsed.payload)
                    break

                case TRANSPORT_OPCODES.PONG:
                    break

                default:
                    throw new Error(`Unknown transport opcode: ${parsed.opcode}`)
            }
        } catch (e) {
            closeAndReportFailure('Failed to handle ESP32 frame: ' + e.message)
            return
        }
    }
}

async function cleanupTransportState() {
    const port = serialPort
    const reader = serialReader
    const writer = serialWriter

    serialPort = null
    serialReader = null
    serialWriter = null
    readLoopPromise = null

    debugSerial('[JS] cleaning up transport state')

    if (reader) {
        try { await reader.cancel() } catch (_) { }
        try { reader.releaseLock() } catch (_) { }
    }

    if (writer) {
        try { writer.releaseLock() } catch (_) { }
    }

    if (port) {
        try { await port.close() } catch (_) { }
    }

    logNoiseBytes(new Uint8Array(0), true)
}

function finalizeClosedTransport() {
    const wasExplicit = explicitDisconnectRequested
    const hadHandshake = hasCompletedHandshake
    const closeReason = pendingCloseReason

    resetProtocolState()
    explicitDisconnectRequested = false

    debugSerial('[JS] transport closed', {
        wasExplicit,
        hadHandshake,
        closeReason,
    })

    if (wasExplicit) {
        app.ports.onDisconnectSuccessful.send(null)
        return
    }

    if (hadHandshake) {
        app.ports.onDisconnectSuccessful.send(null)
        complainNonFatal(closeReason || 'Serial transport disconnected')
    } else {
        complainNonFatal(closeReason || 'Failed to connect to ESP32 over serial')
    }
}

async function disconnectTransportInternal() {
    await cleanupTransportState()
    finalizeClosedTransport()
}

async function startReadLoop(port) {
    //debugSerial('[JS reader] starting read loop')
    try {
        while (serialPort === port && serialReader) {
            const { value, done } = await serialReader.read()
            if (done) {
                //debugSerial('[JS reader] read loop done')
                break
            }
            if (!value || value.length === 0) {
                continue
            }

            // Too much logging:
            // debugSerial('[JS reader] read bytes from serial', { chunkBytes: value.length })
            transportReadBuffer = appendBytes(transportReadBuffer, value)
            processTransportReadBuffer()
        }
    } catch (e) {
        if (serialPort === port && !explicitDisconnectRequested) {
            console.error('[Serial] read loop error', e)
            pendingCloseReason = pendingCloseReason || ('Serial transport error: ' + e.message)
        }
    } finally {
        if (serialPort === port) {
            await cleanupTransportState()
            finalizeClosedTransport()
        }
    }
}

async function connectTransport() {
    if (serialPort) {
        complainNonFatal('Cannot connect: already connected')
        return
    }

    if (!('serial' in navigator)) {
        complainNonFatal('Web Serial is not available in this browser')
        return
    }

    resetProtocolState()
    explicitDisconnectRequested = false
    //debugSerial('[JS] connect requested')

    let port
    try {
        port = await navigator.serial.requestPort()
        //debugSerial('[JS] serial port selected')
    } catch (e) {
        if (e.name !== 'NotFoundError') {
            complainNonFatal('Failed to select serial port: ' + e.message)
        }
        return
    }

    try {
        await port.open({
            baudRate: SERIAL_BAUD_RATE,
            bufferSize: SERIAL_BUFFER_SIZE,
        })
        //debugSerial('[JS] serial port opened', {
        //    baudRate: SERIAL_BAUD_RATE,
        //    bufferSize: SERIAL_BUFFER_SIZE,
        //})
    } catch (e) {
        complainNonFatal('Failed to open serial port: ' + e.message)
        return
    }

    try {
        serialPort = port
        serialReader = port.readable.getReader()
        serialWriter = port.writable.getWriter()
        readLoopPromise = startReadLoop(port)
        await sendAppFrame(FRAME_TYPES.GET_ESP32_DATA, new Uint8Array(0))
        debugSerial('[JS writer] GET_ESP32_DATA sent')
    } catch (e) {
        pendingCloseReason = 'Failed to request ESP32 data: ' + e.message
        await disconnectTransportInternal()
    }
}

async function disconnectTransport() {
    if (!serialPort) {
        resetProtocolState()
        explicitDisconnectRequested = false
        debugSerial('[JS] disconnect requested while already disconnected')
        app.ports.onDisconnectSuccessful.send(null)
        return
    }

    explicitDisconnectRequested = true
    debugSerial('[JS] disconnect requested')

    try { await writeTransportFrame(TRANSPORT_OPCODES.CLOSE, new Uint8Array(0)) } catch (_) { }

    await disconnectTransportInternal()
}

app.ports.connect.subscribe(() => {
    void connectTransport()
})

app.ports.disconnect.subscribe(() => {
    void disconnectTransport()
})

app.ports.sendRootNode.subscribe((commandBytes) => {
    if (!serialPort || !serialWriter) {
        complainNonFatal('Cannot send root node: not connected')
        return
    }

    if (isWaitingForAck) {
        complainNonFatal('Cannot send root node: waiting for ACK');
        return;
    }

    const bytes = new Uint8Array(commandBytes.buffer, commandBytes.byteOffset, commandBytes.byteLength);

    isWaitingForAck = true

    void sendAppFrame(FRAME_TYPES.SET_ROOT_NODE, bytes)
        .then(() => { })
        .catch((e) => {
            isWaitingForAck = false
            complainNonFatal('Failed to send root node: ' + e.message)
        })
})
