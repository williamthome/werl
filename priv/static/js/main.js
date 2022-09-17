const app = document.getElementById("app")
const msgs = document.getElementById("msgs")
const msgForm = document.getElementById("msg-form")

const state = new Proxy({
    onlineCount: 0
}, {
    get: function (target, name) {
        return target[name]
    },
    set: function (obj, prop, value) {
        obj[prop] = value;
        return true
    },
})

msgForm.addEventListener("submit", (e) => {
    e.preventDefault()

    const msg = new FormData(msgForm).get("msg")

    const msgElem = document.createElement("div")
    msgElem.innerText = msg
    msgs.appendChild(msgElem)

    werl.broadcast("/chat", msg)

    msgForm.reset()
})

function increment() {
    werl.cast("increment")
}

function callback() {
    werl.call("callback", (msg) => {
        console.log("Got callback", msg)
    })
}

function callback2() {
    werl.call("callback", (msg) => {
        console.log("Got callback2", msg)
    })
}

/* WErl
   ---------------------------------------------------------------------------*/

const werl = buildWerl(app)

werl.join("/chat", {
    onjoined: ({yourself, payload: someone}) => {
        if (yourself) {
            console.info("Your invite to the chat was accepted.")
            werl.on("/chat", ({yourself, payload: msg}) => {
                !yourself && console.log("Msg received via '/chat'", msg)
            })
        } else {
            console.info(someone, "joined the chat")
        }
    },
    onleft: ({yourself, payload: someone}) => {
        yourself
            ? console.info("You left the chat")
            : console.info(someone, "left the chat")
    },
    onrefused: () => alert("Not allowed to join the chat."),
    onmsg: ({yourself, payload: msg}) => {
        !yourself && console.info("Received chat msg:", msg)
    },
})
