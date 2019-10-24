const { fibonacci } = require('./out/js/fibonacci');
const fs = require('fs')
const bufferSource = fs.readFileSync('./out/wasm/fibonacci.wasm')
const promise = WebAssembly.instantiate(new Uint8Array(bufferSource));
var Benchmark = require('benchmark');
var suite = new Benchmark.Suite;


promise.then(lib =>
    suite
        .add('JS#fibonacci', function () {
            fibonacci(20)
        })
        .add('Wasm#fibonacci', async function () {
            lib.instance.exports.fibonacci(20)
        })
        .on('cycle', function (event) {
            console.log(String(event.target));
        })
        .on('complete', function () {
            console.log('Fastest is ' + this.filter('fastest').map('name'));
        })
        .run({ 'async': true })
);