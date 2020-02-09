const fs = require("fs");

function wasm(source) {
  const bufferSource = fs.readFileSync(source);
  const promise = WebAssembly.instantiate(new Uint8Array(bufferSource));
  return promise;
}

const jsFibRec = require("./out/js/fibonacci").fibonacci;
const wasmFibRec = wasm("./out/wasm/fibonacci.wasm");
const jsFibList = require("./out/js/fibonacci_list").fibonacci;
const wasmFibList = wasm("./out/wasm/fibonacci_list.wasm");
const jsGcd = require("./out/js/gcd").gcd;
const wasmGcd = wasm("./out/wasm/gcd.wasm");

test("JS calculates fibonacci 7", () => {
  expect(jsFibRec(7)).toMatchInlineSnapshot(`21`);
});

test("WASM calculates fibonacci 0", async () => {
  const lib = await wasmFibRec;
  expect(lib.instance.exports.fibonacci(0)).toMatchInlineSnapshot(`1`);
});
test("WASM calculates fibonacci 1", async () => {
  const lib = await wasmFibRec;
  expect(lib.instance.exports.fibonacci(1)).toMatchInlineSnapshot(`1`);
});
test("WASM calculates fibonacci 2", async () => {
  const lib = await wasmFibRec;
  expect(lib.instance.exports.fibonacci(2)).toMatchInlineSnapshot(`2`);
});
test("WASM calculates fibonacci 7", async () => {
  const lib = await wasmFibRec;
  expect(lib.instance.exports.fibonacci(7)).toMatchInlineSnapshot(`21`);
});

test("JS calculates fibonacci 7 using lists", () => {
  expect(jsFibList(7)).toMatchInlineSnapshot(`21`);
});

test("WASM calculates fibonacci 7 using lists", async () => {
  const lib = await wasmFibList;
  expect(lib.instance.exports.fibonacci(7)).toMatchInlineSnapshot(`21`);
});

test("JS calculates gcd(6, 10)", () => {
  expect(jsGcd(6, 10)).toMatchInlineSnapshot(`2`);
});

test("WASM calculates gcd(6, 10)", async () => {
  const lib = await wasmGcd;
  expect(lib.instance.exports.gcd(6, 10)).toMatchInlineSnapshot(`2`);
});
