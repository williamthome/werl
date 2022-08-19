/* Helpers
   -------------------------------------------------------------------------- */

/**
 *
 * @param {string} eventName
 * @param {{}} [payload]
 * @returns {{}}
 */
function buildData(eventName, payload = {}) {
    return {
        event: eventName,
        payload
    }
}

/* State
   -------------------------------------------------------------------------- */

const STATUS_READY = "ready"
const STATUS_NOT_READY = "not_ready"

const state = new Proxy({
    status: STATUS_NOT_READY,
}, {
    get: function (target, name) {
        return target[name]
    },
    set: function (obj, prop, value) {
        obj[prop] = value;
        return true
    },
})

/* Worker
   -------------------------------------------------------------------------- */

const scriptURL = "js/worker.js"
const worker = new Worker(scriptURL)

worker.onmessage = function (e) {
    const { event, payload } = msg = e.data
    switch (event) {
        case "ready":
            state.status = STATUS_READY
            break
        default:
            throw new Error(event
                ? "Event not implemented: " + event
                : "Event is required"
            )
    }
}

console.log("Worker is ready")

/* Socket
   -------------------------------------------------------------------------- */

/** @type {WebSocket|undefined} */
let socket = undefined
const msgQueue = []
let resolvingSocket = true

async function openSocketConnection() {
    if (!("WebSocket" in window)) {
        throw new Error("WebSocket is not supported by this browser")
    }

    resolvingSocket = true

    const socketResolve = new Promise((resolve) => {
        const protocol = "ws"
        const host = location.host
        const uri = "/websocket"
        const url = `${protocol}://${host}${uri}`
        socket = new WebSocket(url)

        socket.onopen = function () {
            console.log("Socket is connected")
            return resolve()
        }

        socket.onmessage = function (e) {
            const msg = e.data
            worker.postMessage(msg)
        }

        socket.onclose = function () {
            console.log("Socket connection closed")
            return resolve()
        }

        console.log("Socket is ready")
    })

    await socketResolve

    resolvingSocket = false
}

function send(eventName, payload = {}) {
    const data = buildData(eventName, payload)
    const msg = JSON.stringify(data)

    // TODO: Msg monitor
    if (!socket) {
        msgQueue.push({ msg })
        return
    }

    socket.send(msg)
}

openSocketConnection().then(() => {
    if (socket.readyState !== WebSocket.OPEN) return

    state.status = STATUS_READY

    send("ready")
})

/* Events
   -------------------------------------------------------------------------- */

// Not implemented yet!
