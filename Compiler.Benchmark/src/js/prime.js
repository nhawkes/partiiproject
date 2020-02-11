function *from(n){
    yield n
    yield* from(n+1)
}
function *sieve(xs){
    const p = xs.next()
    yield p.value
    yield* sieve(filter(n => nonMultiple(p.value, n), xs))
}
function *filter(predicate, xs){
    const x = xs.next()
    if(predicate(x.value)){
        yield x.value
    }
    yield* filter(predicate, xs)
}
function nonMultiple(p, n){ 
    return ((~~(n/p))*p) !== n ;
}
function take(i, list){
    if (i === 0)
        return list.next().value
    else
        list.next()
    return(take(i-1, list))

}

/**
 * @param {number} n
 */
export function prime(n) {
    return take(n, sieve(from(2)))
}