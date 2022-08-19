/* Helpers
   -------------------------------------------------------------------------- */

function assign(static, index, value, recursionCount) {
    return static.splice(parseInt(index) + 1 + recursionCount, 0, value)
}

function patch({ static, ...args }) {
    const staticClone = [...static]

    let recursionCount = 0
    Object.entries(args).forEach(([index, value]) => {
        assign(staticClone, index, value, recursionCount)
        recursionCount++
    })

    return staticClone.join("")
}

/* API
   -------------------------------------------------------------------------- */

const app = document.getElementById("app")
const { static, dynamic, bindings, indexes } = window.werl
let count = bindings.count

function render(static, dynamic) {
    const html = patch({
        static,
        ...dynamic
    })
    morphdom(app, html)
}

// TODO: Sever side increment
function increment() {
    count++

    const countValues = indexes.count.reduce((acc, index) => {
        return { ...acc, [index]: count };
    }, {});

    render(static, countValues)
}
