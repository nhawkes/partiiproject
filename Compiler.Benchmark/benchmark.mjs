import * as libBaseline from './out/wasm/baseline.wasm'
import fibonacciJs from './out/js/fibonacci.js'
import * as fibonacciWasm from './out/wasm/fibonacci.wasm'
import fibonacciListJs from './out/js/fibonacci_list.js'
import * as fibonacciListWasm from './out/wasm/fibonacci_list.wasm'
import gcdJs from './out/js/gcd.js'
import * as gcdWasm from './out/wasm/gcd.wasm'
import primeJs from './out/js/prime.js'
import * as primeWasm from './out/wasm/prime.wasm'
import Benchmark from 'benchmark';

new Benchmark.Suite("Fibonacci")
    .add('Wasm', function () {
        libBaseline.fibonacci(20)
    })
    .add('JS', function () {
        fibonacciJs.fibonacci(20)
    })
    .add('Compiler', function () {
        fibonacciWasm.fibonacci(20)
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
    .on('cycle', function (event) {
        console.log(`${this.name}#${String(event.target)}`);
    })
    .on('complete', function () {
        console.log(`Fastest ${this.name} is ${this.filter('fastest').map('name')}`);
    })
    .run({ 'async': false }) 

new Benchmark.Suite("GCD")   
    .add('JS', function () {
        gcdJs.gcd(1134903170,1836311903)
    })
    .add('Compiler', function () {
        gcdWasm.gcd(1134903170,1836311903)
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
    .on('cycle', function (event) {
        console.log(`${this.name}#${String(event.target)}`);
    })
    .on('complete', function () {
        console.log(`Fastest ${this.name} is ${this.filter('fastest').map('name')}`);
    })
    .run({ 'async': false }) 