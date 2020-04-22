import * as libBaselineFib from './out/wasm/fib-baseline.wasm'
import * as libBaselineGcd from './out/wasm/gcd-baseline.wasm'
import fibonacciJs from './out/js/fibonacci.js'
import * as fibonacciWasm from './out/wasm/fibonacci.wasm'
import * as fibonacciWasmOpt from './out/wasm/fibonacci-optimised.wasm'
import fibonacciListJs from './out/js/fibonacci_list.js'
import * as fibonacciListWasm from './out/wasm/fibonacci_list.wasm'
import * as fibonacciListWasmOpt from './out/wasm/fibonacci_list-optimised.wasm'
import gcdJs from './out/js/gcd.js'
import * as gcdWasm from './out/wasm/gcd.wasm'
import * as gcdWasmOpt from './out/wasm/gcd-optimised.wasm'
import primeJs from './out/js/prime.js'
import * as primeWasm from './out/wasm/prime.wasm'
import * as primeWasmOpt from './out/wasm/prime-optimised.wasm'
import Benchmark from 'benchmark';

new Benchmark.Suite("Fibonacci")
    .add('Baseline', function () {
        libBaselineFib.fibonacci(20)
    })
    .add('JS', function () {
        fibonacciJs.fibonacci(20)
    })
    .add('Compiler', function () {
        fibonacciWasm.fibonacci(20)
    })
    .add('CompilerOptimised', function () {
        fibonacciWasmOpt.fibonacci(20)
    })
    .on('cycle', function (event) {
        console.log(`${this.name}#${String(event.target)}`);
    })
    .on('complete', function () {
        console.log(`Fastest ${this.name} is ${this.filter('fastest').map('name')}`);
    })
    .run({ 'async': false })

new Benchmark.Suite("Fibonacci List")
    .add('JS', function () {
        fibonacciListJs.fibonacci(400)
    })
    .add('Compiler', function () {
        fibonacciListWasm.fibonacci(400)
    })
    .add('CompilerOptimised', function () {
        fibonacciListWasmOpt.fibonacci(400)
    })
    .on('cycle', function (event) {
        console.log(`${this.name}#${String(event.target)}`);
    })
    .on('complete', function () {
        console.log(`Fastest ${this.name} is ${this.filter('fastest').map('name')}`);
    })
    .run({ 'async': false })

new Benchmark.Suite("GCD")
    .add('JS', function () {
        gcdJs.gcd(1134903170, 1836311903)
    })
    .add('Baseline', function () {
        libBaselineGcd.gcd(1134903170, 1836311903)
    })
    .add('Compiler', function () {
        gcdWasm.gcd(1134903170, 1836311903)
    })
    .add('CompilerOptimised', function () {
        gcdWasmOpt.gcd(1134903170, 1836311903)
    })
    .on('cycle', function (event) {
        console.log(`${this.name}#${String(event.target)}`);
    })
    .on('complete', function () {
        console.log(`Fastest ${this.name} is ${this.filter('fastest').map('name')}`);
    })
    .run({ 'async': false })

new Benchmark.Suite("Prime")
    .add('JS', function () {
        primeJs.prime(20)
    })
    .add('Compiler', function () {
        primeWasm.prime(20)
    })
    .add('CompilerOptimised', function () {
        primeWasmOpt.prime(20)
    })
    .on('cycle', function (event) {
        console.log(`${this.name}#${String(event.target)}`);
    })
    .on('complete', function () {
        console.log(`Fastest ${this.name} is ${this.filter('fastest').map('name')}`);
    })
    .run({ 'async': false }) 