/* Helpers
   -------------------------------------------------------------------------- */

// function assign(static, index, value, recursionCount) {
//     return static.splice(parseInt(index) + 1 + recursionCount, 0, value)
// }

// function patch({ static, ...args }) {
//     const staticClone = [...static]

//     let recursionCount = 0
//     Object.entries(args).forEach(([index, value]) => {
//         assign(staticClone, index, value, recursionCount)
//         recursionCount++
//     })

//     return staticClone.join("")
// }

/* API
   -------------------------------------------------------------------------- */

const app = document.getElementById("app")
// const { static, dynamic, bindings, indexes } = window.werl

// function render(payload) {
    // const dynamic = Object.entries(payload).reduce((acc1, [prop, value]) => {
    //     return { ...acc1, ...indexes[prop].reduce((acc2, index) => {
    //         return { ...acc2, [index]: value }
    //     }, acc1)}
    // }, {})

    // const html = patch({
    //     static,
    //     ...dynamic
    // })
// TODO: Reduce websocket msg size
function render(html) {
    morphdom(app, html)
}

function increment() {
    // render({ count: ++bindings.count })
    send("increment")
}
