(module

(func $gcd (export "gcd") (param $p0 i32) (param $p1 i32) (result i32)
    get_local $p0
    get_local $p1
    i32.eq
    i32.const 0
    i32.eq
    if $I0 (result i32)
      get_local $p0
      get_local $p1
      i32.lt_s
      i32.const 0
      i32.eq
      if $I1 (result i32)
        get_local $p1
        get_local $p0
        get_local $p1
        i32.sub
        call $gcd
      else
        get_local $p1
        get_local $p0
        call $gcd
      end
    else
      get_local $p0
    end)
)