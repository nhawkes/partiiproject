/**
 * @param {number} n
 */
export function gcd(a, b) {
    if (a==b) return a
    if (a<b) 
        return gcd(b, a)
    else 
        return gcd(b, a-b)
}