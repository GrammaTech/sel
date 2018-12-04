var o = { first: "John" };

with(o) {
    console.log("Hello "+first);
}
