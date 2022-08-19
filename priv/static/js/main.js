/* Helpers
   -------------------------------------------------------------------------- */

function assign(s, index, value) {
    return s.splice(index + 1, 0, value)
}

function patch({ s, ...args }) {
    const c = [...s]
    Object.entries(args).forEach(([index, value]) => {
        assign(c, index, value)
    })
    return c.join("")
}

/* API
   -------------------------------------------------------------------------- */

const app = document.getElementById("app")
let count = 0

function render({ count }) {
    const html = patch({
        s: [
            // TODO: Static from server
            `<div id="app">
                <button onclick="increment();">Increment</button>
                <div>Count:<span id="count">`, `</span></div>
            </div>`
        ],
        0: count
    })
    morphdom(app, html)
}

function increment() {
    render({ count: ++count })
}
