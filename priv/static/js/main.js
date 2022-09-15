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

werl.join("/chat", ({joined, yourself, payload: someone}) => {
    if (!joined) return alert("Not allowed to join the chat.")

    if (yourself) {
        console.info("Your invite to the chat was accepted.")
        werl.on("/chat", ({yourself, payload: msg}) => {
            !yourself && console.log("Msg received via '/chat'", msg)
        })
    } else {
        console.info(someone, "joined the chat")
    }
})
