const { fibonacci } = require("./out/js/fibonacci");
const fs = require("fs");
const bufferSource = fs.readFileSync("./out/wasm/fibonacci.wasm");
const promise = WebAssembly.instantiate(new Uint8Array(bufferSource));

test("JS calculates fibonacci 7", () => {
  expect(fibonacci(7)).toMatchInlineSnapshot(`21`);
});

test("WASM calculates fibonacci 7", async () => {
  const lib = await promise;
  expect(lib.instance.exports.fibonacci(7)).toMatchInlineSnapshot(`21`);
});
