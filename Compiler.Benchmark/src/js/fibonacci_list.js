
function *fibonacciListFrom(a, b){
    yield a
    yield* fibonacciListFrom(b, a+b) 
}
function take(i, list){
    if (i === 0)
        return list.next().value
    else
        list.next()
    return(take(i-1, list))

}
export function fibonacci(x){
    return take(x, fibonacciListFrom(1, 1))
}