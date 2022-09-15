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

werl.on("accepted", (topic) => {
    switch(topic) {
        case "/chat":
            console.info("Your invite to the chat was accepted.")
            break
    }
})

werl.on("refused", (topic) => {
    switch(topic) {
        case "/chat":
            alert("Not allowed to join the chat.")
            break
    }
})

werl.on("joined", ({topic, payload: someone}) => {
    switch(topic) {
        case "/chat":
            console.info(someone, "joined the chat")
            break
    }
})

werl.join("/chat", (msg) => {
    console.log("Msg received via '/chat'", msg)
})
