import fibonacci from './out/js/fibonacci.js'
import * as libBaseline from './out/wasm/baseline.wasm'
import * as libCompiler from './out/wasm/fibonacci.wasm'
import Benchmark from 'benchmark';
var suite = new Benchmark.Suite;

suite
    .add('JS#fibonacci', function () {
        fibonacci(20)
    })
    .add('Wasm#fibonacci', function () {
        libBaseline.fibonacci(20)
    })
    .add('Compiler#fibonacci', function () {
        libCompiler.fibonacci(20)
    })
    .on('cycle', function (event) {
        console.log(String(event.target));
    })
    .on('complete', function () {
        console.log('Fastest is ' + this.filter('fastest').map('name'));
    })
    .run({ 'async': true })