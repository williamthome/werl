const app = document.getElementById("app")
const werl = buildWerl(app)

function increment() {
    werl.cast("increment")
}
