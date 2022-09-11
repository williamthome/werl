function buildWerl(root, topic = "/") {

    /* State
    --------------------------------------------------------------------------*/

    const state = new Proxy({
        static: window.werlStatic
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
    --------------------------------------------------------------------------*/

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
    --------------------------------------------------------------------------*/

    function buildWErlSocket() {
        /** @type {WebSocket|undefined} */
        let socket = undefined
        const subscribers = []
        const msgQueue = []

        function connect() {
            return new Promise((resolve) => {
                const protocol = "ws"
                const host = location.host
                const uri = "/websocket"
                const url = `${protocol}://${host}${uri}${topic}`
                socket = new WebSocket(url)

                socket.onopen = async function () {
                    console.log("WErl socket is connected")
                    await flush()
                    return resolve()
                }

                socket.onmessage = async function (e) {
                    const msg = e.data
                    const {event, payload} = JSON.parse(msg)
                    await notify(event, payload)
                }

                socket.onclose = function (e) {
                    console.log("WErl socket connection closed", e)
                    socket = undefined
                    return resolve()
                }
            })
        }

        function disconnect() {
            const code = 1000 // normal
            const reason = "WErl socket disconnected"
            socket.close(code, reason)
        }

        function on(event, handler) {
            subscribers.push({ event, handler })
        }

        async function cast(event, payload = {}) {
            const data = {event, payload}
            const msg = JSON.stringify(data)
            msgQueue.push(new Promise((resolve, reject) => {
                try {
                    socket?.readyState === WebSocket.OPEN && socket.send(msg)
                    resolve()
                } catch(err) {
                    reject(err)
                }
            }))
            await flush()
        }


        async function flush() {
            (!socket || !socket.readyState === WebSocket.CLOSED) && await connect()

            await Promise.allSettled(msgQueue)
        }

        async function notify(event, payload) {
            const notifyQueue = subscribers.reduce((acc, subs) => {
                subs.event === event && acc.push(
                    new Promise(async (resolve, reject) => {
                        try {
                            await subs.handler(payload)
                            resolve()
                        } catch (err) {
                            reject(err)
                        }
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
    --------------------------------------------------------------------------*/

    const werlDom = buildDOM()
    let werlSocket

    if ("WebSocket" in window) {
        werlSocket = buildWErlSocket(topic)

        werlSocket.on("ready", () => {
            console.log("WErl socket is ready")
        })

        werlSocket.on("render", (bindings) => {
            werlDom.render(root, state.static, bindings)
        })

        werlSocket.connect()
    } else {
        const errorMsg = "WebSocket is not supported by this browser"
        werlDom.render(root, errorMsg)
    }

    const doesNothing = () => {}

    console.log("WErl built")

    return {
        connect: werlSocket?.connect ?? doesNothing,
        disconnect: werlSocket?.disconnect ?? doesNothing,
        on: werlSocket?.on ?? doesNothing,
        cast: werlSocket?.cast ?? doesNothing,
    }
}
