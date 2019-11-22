const { fibonacci } = require("./out/js/fibonacci");
const fs = require("fs");
const bufferSource = fs.readFileSync("./out/wasm/fibonacci.wasm");
const promise = WebAssembly.instantiate(new Uint8Array(bufferSource));

test("JS calculates fibonacci 7", () => {
  expect(fibonacci(7)).toMatchInlineSnapshot(`21`);
});

test("WASM calculates fibonacci 0", async () => {
  const lib = await promise;
  expect(lib.instance.exports.fibonacci(0)).toMatchInlineSnapshot(`1`);
});
test("WASM calculates fibonacci 1", async () => {
  const lib = await promise;
  expect(lib.instance.exports.fibonacci(1)).toMatchInlineSnapshot(`1`);
});
test("WASM calculates fibonacci 2", async () => {
  const lib = await promise;
  expect(lib.instance.exports.fibonacci(2)).toMatchInlineSnapshot(`2`);
});
test("WASM calculates fibonacci 7", async () => {
  const lib = await promise;
  expect(lib.instance.exports.fibonacci(7)).toMatchInlineSnapshot(`21`);
});
