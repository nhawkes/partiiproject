(module

(func $fibonacci (export "fibonacci") (param $n i32) (result i32)
    (if (result i32)
        (i32.eq
            (get_local 0)
            (i32.const 0)
        )
        (then
            (i32.const 1)           
        )
        (else
            (if (result i32)
                (i32.eq 
                    (get_local 0)
                    (i32.const 1)
                )
                (then
                     (i32.const 1)                
                )
                (else
                    (i32.add
                        (call $fibonacci (i32.sub (get_local $n) (i32.const 1)))
                        (call $fibonacci (i32.sub (get_local $n) (i32.const 2)))
                    )                
                )       
            ) 
        )    
    )
)
)