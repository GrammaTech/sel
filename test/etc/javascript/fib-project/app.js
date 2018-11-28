const fib = require('./fib.js');
const argparse = require('argparse');

var parser = argparse.ArgumentParser({
    version: '1.0',
    addHelp: true,
    description: 'Fibonacci example'})
parser.addArgument('nums', {
    help: 'Numbers to compute fibonacci numbers for',
    nargs: '+',
    type: 'int',
    metavar: 'N'})

var args = parser.parseArgs()
for (var i = 0; i < args.nums.length; i++) {
    console.log('fib %d: %d', args.nums[i], fib.fibonacci(args.nums[i]));
}
