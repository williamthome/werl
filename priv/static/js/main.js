const app = document.getElementById("app")
const werl = buildWerl(app)

werl.on("render", () => {
    console.log("Received render")
})

function increment() {
    werl.cast("increment")
}
