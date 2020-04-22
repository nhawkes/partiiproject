module Generators
open Core
open FsCheck
let core() =
    let rec coreSized n =
        match n with
        |0 ->
            Gen.oneof[
                Gen.choose(0, n) |>
                    Gen.map (fun x -> B(x,0)) |>
                    Gen.map Var
                Arb.from<Lit>.Generator |>
                    Gen.map Lit |>
                    Gen.map Const
                Arb.from<Binds<_>>.Generator |>
                    Gen.map(fun binds -> Let(Rec, binds, Closed Core.Unreachable))    

            ]            
        |_ ->
            Gen.oneof[
                Gen.map2 (fun x y -> Lam(("", y), Closed x))
                    <| coreSized (n-1)
                    <| Arb.from<_>.Generator                
                Gen.map2(fun e v -> Case(e, ("", v), []))  
                    <| coreSized (n-1)
                    <| Arb.from<_>.Generator         
            ]


    Gen.sized coreSized    

type Generators =
  static member Position() =
      {new Arbitrary<FParsec.Position>() with
          override x.Generator = Gen.constant <| FParsec.Position("",0L,0L,0L)
      }
  static member CoreArb() =
      {new Arbitrary<Core.Expr<_>>() with
        override x.Generator = core()
      }
  static member NoNullString() : Arbitrary<string> =
        Arb.convert (fun (x:StringNoNulls)-> if isNull(x.Get) then "" else x.Get) (StringNoNulls) (Arb.Default.StringWithoutNullChars())