module Parser


open Xunit

let ``Parses Prime``()=
    let prime = """
        data Cons(head, tail)
        data Empty()
        take(i, list) = {
            return switch(list){
                | Cons(head, tail) =>
                    {
                        return switch(i){
                            | 0 => head
                            | _ => take(i-1, tail)
                        }
                    }
            }
        }
        from(x) = {
            next = x+1
            return Cons(x, from(next))
        }

        sieve(list) = {
            return switch(list){
                | Cons(prime, tail) => {
                    return Cons(prime, sieve(filter(nonMultiple(prime), tail)))
                }
                |Empty() => Empty()
            }
        }
        filter(predicate(x), list) = {
            return switch(list){
                | Cons(head, tail) => {
                    rest = filter(predicate, tail)
                    return if(predicate(head)) then Cons(head, rest) else rest
                }
                |Empty() => Empty()
            }  
        }
        nonMultiple(p, n) = {
            return if ((n/p)*p) = n then
                false
            else
                true
        }  
        yes(x) = {return true}
        export prime(x) = {
            list = from(2)
            filteredList = sieve(list)
            return take(x, filteredList)
        }
        """
    let passes =
        match Parser.parseString prime with
        | Error err -> false
        | Ok astModule -> true

    Assert.True(passes)