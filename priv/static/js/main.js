const app = document.getElementById("app")
const chatMsgTxtArea = document.getElementById("chat-msg")
const sendChatMsgBtn = document.getElementById("send-chat-msg")

chatMsgTxtArea.addEventListener("input", maybeEnableSendMsgBtn)

function sendChatMsg() {
    werl.broadcast("/chat", chatMsgTxtArea.value)
}

function increment() {
    werl.cast("increment")
}

function maybeEnableSendMsgBtn() {
    sendChatMsgBtn.disabled = !chatMsgTxtArea.value
}

/* WErl
   ---------------------------------------------------------------------------*/

const werl = buildWerl(app)

werl.on("ready", maybeEnableSendMsgBtn)

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
})
