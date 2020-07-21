import "module-name"
export { f, g }

async function f() {
    var a = 1, b = 0, temp;
    temp = a + b // comment
    console.log("Hello world!"); /* comment 2 */
    debugger;
    await f()
    temp = await f();
    if (a == 1) {
        f()
    } else {
        return 0
    }
}

function* g() {
    yield 0
}
