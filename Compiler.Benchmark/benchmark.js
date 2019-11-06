const { fibonacci } = require('./out/js/fibonacci');
const fs = require('fs')
const bufferSourceBaseline = fs.readFileSync('./out/wasm/baseline.wasm')
const promiseBaseline = WebAssembly.instantiate(new Uint8Array(bufferSourceBaseline));
const bufferSourceCompiler = fs.readFileSync('./out/wasm/fibonacci.wasm')
const promiseCompiler = WebAssembly.instantiate(new Uint8Array(bufferSourceCompiler));
var Benchmark = require('benchmark');
var suite = new Benchmark.Suite;

Promise.all([promiseBaseline, promiseCompiler]).then(([libBaseline, libCompiler]) =>
    suite
        .add('JS#fibonacci', function () {
            fibonacci(20)
        })
        .add('Wasm#fibonacci', async function () {
            libBaseline.instance.exports.fibonacci(20)
        })
        .add('Compiler#fibonacci', async function () {
            libCompiler.instance.exports.fibonacci(20)
        })
        .on('cycle', function (event) {
            console.log(String(event.target));
        })
        .on('complete', function () {
            console.log('Fastest is ' + this.filter('fastest').map('name'));
        })
        .run({ 'async': true })
);