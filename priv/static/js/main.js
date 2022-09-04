/* Helpers
   -------------------------------------------------------------------------- */

function assign(static, index, value, recursionCount) {
    return static.splice(parseInt(index) + recursionCount, 0, value)
}

function patch({ static, indexes }) {
    const staticClone = [...static]

    let recursionCount = 0
    Object.entries(indexes).forEach(([index, value]) => {
        assign(staticClone, index, value, recursionCount)
        recursionCount++
    })

    return staticClone.join("")
}

/* API
   -------------------------------------------------------------------------- */

const app = document.getElementById("app")

function render(static, indexes) {
    const html = patch({static, indexes})
    morphdom(app, html)
}

function increment() {
    send("increment")
}
