
function* fibonacciListFrom(a, b) {
    while (true) {
        yield a
        const temp = a
        a = b
        b = temp + b
    }
}
function take(n, list) {
    for (var i = 0; i < n; i++)
        list.next()

    return list.next().value

}
export function fibonacci(x) {
    return take(x, fibonacciListFrom(1, 1))
}