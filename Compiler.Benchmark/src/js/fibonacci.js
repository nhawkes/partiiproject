/**
 * @param {number} n
 */
export function fibonacci(n) {
    switch(n){
        case 0: return 1;
        case 1: return 1;
        default: return fibonacci(n - 1) + fibonacci(n - 2);
    }
}