function* foo() {
    yield 1;
    yield 2;
    yield 3;
}

for(var i = 1; i <= 3; i++) {
    console.log(i);
}

var arr = [1, 2, 3];
for(var i in arr) {
    console.log(arr[i]);
}

for(let val of foo()) {
    console.log(val);
}

var i = 1;
while (i <= 3) {
    console.log(i);
    i++;
}

i = 1;
do {
    console.log(i);
    i++;
} while (i <= 3);
