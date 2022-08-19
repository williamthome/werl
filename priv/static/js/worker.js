self.onmessage = function (e) {
    const receivedMsg = e.data
    if (typeof receivedMsg === "string") {
        const msg = JSON.parse(receivedMsg)
        self.postMessage(msg)
    } else {
        self.postMessage(receivedMsg)
    }
}
