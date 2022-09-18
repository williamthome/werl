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
        const callbacks = []

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
                    return resolve()
                }

                _socket.onmessage = async function (e) {
                    const msg = e.data
                    const {event, payload} = JSON.parse(msg)
                    event === "call"
                        ? await flushCall(payload)
                        : await flushBroadcast(event, payload)
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
            await flushCast()
        }

        async function flushCast() {
            (!_socket || !_socket.readyState === WebSocket.CLOSED) && await connect()

            await Promise.allSettled(msgQueue)
        }

        function call(event, payload, callback) {
            if (typeof payload === "function" && !callback) {
                callback = payload
                payload = {}
            }

            const index = callbacks.findIndex((cbs) => event in cbs)
            if (index === -1) {
                callbacks.push({[event]: [callback]})
            } else {
                const cbs = callbacks[index][event]
                const cbIndex = cbs.findIndex((cb) => cb.toString() == callback.toString())
                cbIndex === -1 && cbs.push(callback)
            }

            const data = {event: "call", payload: {event, payload}}
            const msg = JSON.stringify(data)

            _socket.send(msg)
        }

        async function flushCall({event, payload}) {
            const callbackQueue = callbacks.reduce((acc, cbs) => {
                event in cbs && cbs[event].forEach((cb) => acc.push(
                    new Promise(async (resolve) => {
                        resolve(cb(payload))
                    })
                ))
                return acc
            }, [])
            await Promise.allSettled(callbackQueue)
            callbacks.length = 0
        }

        async function flushBroadcast(event, payload) {
            const handlers = subscribers.reduce((acc, subs) => {
                subs.event === event && acc.push(
                    new Promise(async (resolve) => {
                        resolve(subs.handler(payload))
                    })
                )
                return acc
            }, [])
            await Promise.allSettled(handlers)
        }

        console.log("WErl socket built")

        return { connect, disconnect, on, call, cast }
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
        if (typeof token !== "string" && !callback) {
            callback = token
            token = ""
        }

        const joinCast = () => socket.cast("join", {topic: joinTopic, token})
        state.ready ? joinCast() : socket.on("ready", joinCast)

        const shouldJoinedSubscribe = (callback.onjoined || callback.onmsg)
        shouldJoinedSubscribe && socket.on("joined", ({yourself, payload: {topic, payload}}) => {
            if (topic !== joinTopic) return
            callback.onjoined({yourself, payload})
            yourself && callback.onmsg && socket.on(joinTopic, callback.onmsg)
        })

        callback.onleft && socket.on("left", ({yourself, payload: {topic, payload}}) => {
            if (topic !== joinTopic) return
            callback.onleft({yourself, payload})
        })

        callback.onrefused && socket.on("refused", (topic) => {
            if (topic !== joinTopic) return
            callback.onrefused()
        })
    }

    function broadcast(topic, msg) {
        werl.cast("broadcast", {topic, msg})
    }

    function left(topic) {
        socket.cast("left", topic)
    }

    const doesNothing = () => {}

    console.log("WErl built")

    return {
        connect: socket?.connect ?? doesNothing,
        disconnect: socket?.disconnect ?? doesNothing,
        on: socket?.on ?? doesNothing,
        call: socket?.call ?? doesNothing,
        cast: socket?.cast ?? doesNothing,
        join: socket ? join : doesNothing,
        broadcast: socket ? broadcast : doesNothing,
        left: socket ? left : doesNothing,
    }
}
