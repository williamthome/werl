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
const { static, bindings } = window.werl
let count = bindings.count

function render({ count }) {
    const html = patch({
        s: static,
        0: count
    })
    morphdom(app, html)
}

// TODO: Sever side increment
function increment() {
    render({ count: ++count })
}
