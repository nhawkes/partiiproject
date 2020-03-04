module RuntimeFunctions 

type Var =
    | StgVar of Stg.Var
    | Identity
    | Indirection
    | WhnfEval
    | Malloc
    | Clone
    | Apply
    | This
    | ThisFunction
    | ArgsRemaining

type RuntimeFunction =
    { name: Var
      debugName: Wasm.Name
      debugLocalNames: Wasm.Name list
      functype: Wasm.FuncType
      indirect: bool
      func: Wasm.Func
    }
   
let stdFuncType = [ Wasm.I32 ], [ Wasm.I32 ]
let heapTop = 0u

let identity =
    { name = Identity
      debugName = "identity"
      debugLocalNames= ["x"]
      functype = stdFuncType
      indirect = true
      func = [], [ Wasm.LocalGet 0u ] }

let indirection = 
    { name = Indirection
      debugName = "indirection"
      debugLocalNames= ["thunk"]
      functype = stdFuncType
      indirect = true
      func = [], [ 
        Wasm.LocalGet 0u 
        Wasm.I32Load
          { align = 0u
            offset = 8u }
      ] }

let malloc =
    { name = Malloc    
      debugName = "malloc"
      debugLocalNames= ["size"]
      functype = [ Wasm.I32 ], [ Wasm.I32 ]
      indirect = false
      func =
          [],
          [ [
              
              // Load heapTop + 4
              Wasm.GlobalGet heapTop
              Wasm.I32Const 4
              Wasm.I32Add ]

            // Save heapTop
            [ Wasm.GlobalGet heapTop ]

            // Increment to save size at -1
            [ Wasm.GlobalGet heapTop
              Wasm.LocalGet 0u
              Wasm.I32Const 4
              Wasm.I32Add
              Wasm.I32Add
              Wasm.GlobalSet heapTop ]

            // Grow if needed
            [ Wasm.Block([],
                [
                 Wasm.Loop ([],
                    [  
                       Wasm.MemorySize
                       Wasm.GlobalGet heapTop
                       Wasm.I32Const 16
                       Wasm.I32ShrU
                       Wasm.I32GtU
                       Wasm.BrIf 1u
                       Wasm.I32Const 1
                       Wasm.MemoryGrow
                       Wasm.Drop 
                       Wasm.Br 0u
                    ])
                ])
            ]
            

            // Load heapTop
            [ Wasm.LocalGet 0u
              Wasm.I32Store
                  { align = 0u
                    offset = 0u } ] 

            // Return (old) heapTop + 4          
          ]
          |> List.concat }

let clone mallocIdx =     
    let thisPointer = 0u
    let size = 1u
    let i = 2u
    let cloned = 3u
    { name = Clone
      debugName = "clone"
      debugLocalNames= ["this"; "size"; "i"; "cloned"]
      functype = [ Wasm.I32 ], [ Wasm.I32 ]
      indirect = false
      func =
          [Wasm.I32; Wasm.I32; Wasm.I32; Wasm.I32],
          [ [
              Wasm.LocalGet thisPointer
              Wasm.I32Const -4
              Wasm.I32Add
              Wasm.I32Load
                { align = 0u
                  offset = 0u }
              Wasm.LocalSet size
              // Get size from This

              Wasm.LocalGet size
              Wasm.Call mallocIdx // Call Malloc
              Wasm.LocalSet cloned

              Wasm.I32Const 0
              Wasm.LocalSet i
              //i=0
            ]


            [ Wasm.Block([],
                    [Wasm.Loop ([],
                       [
                       // If(i>=size) break;
                       Wasm.LocalGet i
                       Wasm.LocalGet size
                       Wasm.I32GeU
                       Wasm.BrIf 1u

                        
                       // Destination
                       Wasm.LocalGet cloned
                       Wasm.LocalGet i
                       Wasm.I32Add

                       // Source
                       Wasm.LocalGet thisPointer
                       Wasm.LocalGet i
                       Wasm.I32Add

                       // Copy
                       Wasm.I32Load {align = 0u; offset = 0u}
                       Wasm.I32Store {align = 0u; offset = 0u}

                       // i+=4
                       Wasm.LocalGet i
                       Wasm.I32Const 4
                       Wasm.I32Add
                       Wasm.LocalSet i

                       // Continue
                       Wasm.Br 0u
                       ])
                    ])
            ]            
            [
                Wasm.LocalGet cloned
            ]

              
                    
          ]
          |> List.concat }

let apply stdFuncTypeIdx =     
    let thisPointer = 0u
    let arg = 1u
    let argsRemaining = 2u
    { name = Apply
      debugName = "apply"
      debugLocalNames= ["f"; "arg"; "argsRemaining"]
      functype = [ Wasm.I32; Wasm.I32 ], [ Wasm.I32 ]
      indirect = false
      func =
          [ Wasm.I32 ],
          [
              // Get argsRemaining
              Wasm.LocalGet thisPointer
              Wasm.I32Load
                { align = 0u
                  offset = 0u }
              Wasm.LocalSet argsRemaining                  

              Wasm.LocalGet argsRemaining
              Wasm.LocalGet thisPointer
              Wasm.I32Add
              // Calculate position of next arg

              Wasm.LocalGet arg
              Wasm.I32Store { align = 0u; offset = 4u }
              // Store the next argument

              Wasm.LocalGet argsRemaining
              Wasm.I32Const -4
              Wasm.I32Add
              Wasm.LocalSet argsRemaining
              // Calculate next arg position

              Wasm.LocalGet argsRemaining
              Wasm.I32Eqz
              
              Wasm.IfElse([Wasm.I32], 
                [                    
                    // Function now has enough argument to be called
                    Wasm.LocalGet thisPointer
                    Wasm.LocalGet thisPointer
                    Wasm.I32Load
                      { align = 0u
                        offset = 4u }
                    Wasm.CallIndirect(stdFuncTypeIdx)
                ],
                [
                    
                    // Update next arg pointer
                    Wasm.LocalGet thisPointer
                    
                    Wasm.LocalGet argsRemaining
                    Wasm.I32Store
                      { align = 0u
                        offset = 0u }
                    Wasm.LocalGet thisPointer    
                ]              
              )
          ] }

let whnfEval stdFuncTypeIdx (indirectionIdx:uint32) = 
    let evaluated = 1u
    { name = WhnfEval
      functype = stdFuncType
      debugName = "whnfEval"
      debugLocalNames= ["thunk"]
      indirect = false
      func = [ Wasm.I32 ], [ 
            // Check if thunk and call if so

            [Wasm.LocalGet 0u]
            [Wasm.I32Load
              { align = 0u
                offset = 0u }]
            [Wasm.I32Eqz]
            // Args Remaining = 0

            [Wasm.LocalGet 0u]
            [Wasm.I32Load
              { align = 0u
                offset = 4u }]
            [Wasm.I32Eqz; Wasm.I32Const -1; Wasm.I32Xor]
            // Function != 0

            [Wasm.I32And]
            // Args Remaining = 0 and Function != 0 ie. is a thunk

            [
                Wasm.IfElse([Wasm.I32],
                    [ [Wasm.LocalGet 0u]
                      [Wasm.LocalGet 0u]
                      [ Wasm.I32Load
                          { align = 0u
                            offset = 4u } ]
                      [ Wasm.CallIndirect(stdFuncTypeIdx) ] 
                      [ Wasm.LocalSet evaluated]

                      [ Wasm.LocalGet 0u ]
                      [ Wasm.LocalGet evaluated ]
                      [ Wasm.I32Store
                          { align = 0u
                            offset = 8u } ]

                      [ Wasm.LocalGet 0u ]
                      [ Wasm.I32Const ((indirectionIdx:uint32) |> int32) ]
                      [ Wasm.I32Store
                          { align = 0u
                            offset = 4u } ]

                      [ Wasm.LocalGet evaluated]    
                    ]
                    |> List.concat, [
                        [Wasm.LocalGet 0u]
                    ] |> List.concat
                )
            ]
      ] |> List.concat 
      }