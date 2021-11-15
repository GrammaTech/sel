module.exports = {
    fibonacci: function(num) {
        var a = 1, b = 0, temp;

        while (num > 0) {
            temp = a;
            a = a + b;
            b = temp;
            num--;
        }

        return b;
    }
}
