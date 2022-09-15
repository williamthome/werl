function buildWerl(root) {

    /* State
    --------------------------------------------------------------------------*/

    const state = new Proxy({
        ready: false,
        static: window.werlStatic,
    }, {
        get: function (target, name) {
            return target[name]
        },
        set: function (obj, prop, value) {
            obj[prop] = value;
            return true
        },
    })

    /* DOM
       -----------------------------------------------------------------------*/

    function buildDOM() {
        function assign(static, index, value, recursionCount) {
            return static.splice(parseInt(index) + recursionCount, 0, value)
        }

        function patch(static, indexes) {
            const staticClone = [...static]

            let recursionCount = 0
            Object.entries(indexes).forEach(([index, value]) => {
                assign(staticClone, index, value, recursionCount)
                recursionCount++
            })

            return staticClone.join("")
        }

        function render(elem, static, indexes  = []) {
            const html = patch(static, indexes)
            morphdom(elem, html)
        }

        console.log("WErl DOM built")

        return {
            render
        }
    }

    /* Socket
       -----------------------------------------------------------------------*/

    function buildSocket() {
        /** @type {WebSocket|undefined} */
        let _socket = undefined
        const subscribers = []
        const msgQueue = []

        function connect() {
            return new Promise((resolve) => {
                const protocol = "ws"
                const host = location.host
                const uri = "/websocket"
                const query = `?path=${location.pathname}`
                const url = `${protocol}://${host}${uri}${query}`
                _socket = new WebSocket(url)

                _socket.onopen = async function () {
                    console.log("WErl socket is connected")
                    // await cast("ready", state)
                    return resolve()
                }

                _socket.onmessage = async function (e) {
                    const msg = e.data
                    const {event, payload} = JSON.parse(msg)
                    await notify(event, payload)
                }

                _socket.onclose = function (e) {
                    console.log("WErl socket connection closed", e)
                    _socket = undefined
                    return resolve()
                }
            })
        }

        function disconnect() {
            const code = 1000 // normal
            const reason = "WErl socket disconnected"
            _socket.close(code, reason)
        }

        function on(event, handler) {
            subscribers.push({ event, handler })
        }

        async function cast(event, payload = {}) {
            const data = {event, payload}
            const msg = JSON.stringify(data)
            msgQueue.push(new Promise((resolve) => {
                _socket?.readyState === WebSocket.OPEN
                    ? resolve(_socket.send(msg))
                    : resolve()
            }))
            await flush()
        }


        async function flush() {
            (!_socket || !_socket.readyState === WebSocket.CLOSED) && await connect()

            await Promise.allSettled(msgQueue)
        }

        async function notify(event, payload) {
            const notifyQueue = subscribers.reduce((acc, subs) => {
                subs.event === event && acc.push(
                    new Promise(async (resolve) => {
                        resolve(subs.handler(payload))
                    })
                )
                return acc
            }, [])
            await Promise.allSettled(notifyQueue)
        }

        console.log("WErl socket built")

        return { connect, disconnect, on, cast }
    }

    /* Setup
       -----------------------------------------------------------------------*/

    const dom = buildDOM()
    let socket

    if ("WebSocket" in window) {
        socket = buildSocket()

        socket.on("ready", () => {
            state.ready = true
            console.log("WErl socket is ready")
        })

        socket.on("render", (bindings) => {
            dom.render(root, state.static, bindings)
        })

        socket.connect()
    } else {
        const errorMsg = "WebSocket is not supported by this browser"
        dom.render(root, errorMsg)
    }

    function join(joinTopic, token, callback) {
        if (typeof token === "function" && !callback) {
            callback = token
            token = ""
        }

        const joinCast = () => socket.cast("join", {topic: joinTopic, token})
        state.ready ? joinCast() : socket.on("ready", joinCast)

        if (typeof callback !== "function") return

        socket.on("joined", ({yourself, topic, payload}) => {
            if (topic !== joinTopic) return
            callback({
                joined: true,
                yourself,
                payload
            })
        })

        socket.on("refused", (topic) => {
            if (topic !== joinTopic) return
            callback({
                joined: false,
                yourself: true,
                payload: undefined
            })
        })
    }

    function broadcast(topic, msg) {
        werl.cast("broadcast", {topic, msg})
    }

    const doesNothing = () => {}

    console.log("WErl built")

    return {
        connect: socket?.connect ?? doesNothing,
        disconnect: socket?.disconnect ?? doesNothing,
        on: socket?.on ?? doesNothing,
        cast: socket?.cast ?? doesNothing,
        join: socket ? join : doesNothing,
        broadcast: socket ? broadcast : doesNothing,
    }
}
