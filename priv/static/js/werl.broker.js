function buildWerl(root) {

    /* Helpers
    --------------------------------------------------------------------------*/

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
    --------------------------------------------------------------------------*/

    const STATUS_READY = "ready"
    const STATUS_NOT_READY = "not_ready"

    const state = new Proxy({
        status: STATUS_NOT_READY,
        static: []
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

        function render(elem, static, indexes) {
            const html = patch(static, indexes)
            morphdom(elem, html)
        }

        return {
            render
        }
    }

    /* Worker
    --------------------------------------------------------------------------*/

    function buildWErlWorker(scriptURL, dom, root) {
        const worker = new Worker(scriptURL)
        let _onevent = undefined;

        worker.onmessage = function (e) {
            const { event, payload } = msg = e.data

            const eventName = event === "ready" ? "init" : event

            switch(eventName) {
                case "init":
                    state.status = STATUS_READY
                    state.static = payload
                    break
                case "render":
                    dom.render(root, state.static, payload)
                    break
            }

            _onevent && _onevent(eventName, payload, state)
        }

        console.log("WErl worker is ready")

        return {
            worker,
            set onevent(onevent) {
                _onevent = onevent
            },
        }
    }

    /* Socket
    --------------------------------------------------------------------------*/

    function buildWErlSocket(worker) {
        if (!("WebSocket" in window)) {
            throw new Error("WebSocket is not supported by this browser")
        }

        /** @type {WebSocket|undefined} */
        let socket = undefined
        const msgQueue = []
        let resolvingSocket = true

        async function openSocketConnection() {
            resolvingSocket = true

            const socketResolve = new Promise((resolve) => {
                const protocol = "ws"
                const host = location.host
                const uri = "/websocket"
                const url = `${protocol}://${host}${uri}`
                socket = new WebSocket(url)

                socket.onopen = function () {
                    console.log("WErl socket is connected")
                    return resolve()
                }

                socket.onmessage = function (e) {
                    const msg = e.data
                    worker.postMessage(msg)
                }

                socket.onclose = function () {
                    console.log("WErl socket connection closed")
                    return resolve()
                }

                console.log("WErl socket is ready")
            })

            await socketResolve

            resolvingSocket = false
        }

        openSocketConnection().then(() => {
            if (socket.readyState !== WebSocket.OPEN) return

            if (state.status !== STATUS_READY) {
                state.status = STATUS_READY
                send("ready")
            }
        })

        async function send(eventName, payload = {}) {
            socket.readyState === WebSocket.CLOSED && await openSocketConnection()

            const data = buildData(eventName, payload)
            const msg = JSON.stringify(data)

            // TODO: Msg monitor
            if (!socket.readyState === WebSocket.OPEN) {
                msgQueue.push({ msg })
                return
            }

            socket.send(msg)
        }

        return {
            send
        }
    }

    const werlDom = buildDOM()
    const werlWorker = buildWErlWorker("js/werl.worker.js", werlDom, root)
    const werlSocket = buildWErlSocket(werlWorker.worker)

    return {
        // e.g. werl.handleEvent = (eventName, payload, state) => {}
        set handleEvent(onevent) {
            werlWorker.onevent = onevent
        },
        cast:  werlSocket.send
    }
}
