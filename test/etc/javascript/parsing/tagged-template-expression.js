const myTag = (literals, name) => {
    console.log('Literals', literals); //Output -> Literals [ 'Hello ', '!' ]
    console.log('Interpolation', name); //Output -> Interpolation Steve

    return 'Result from myTag';
};

const name = 'Steve';
const result = myTag `Hello ${name}!`;

console.log(result); //Output -> Result from myTag
