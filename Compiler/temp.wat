(module
  (type $t0 (func (param i32) (result i32)))
  (type $t1 (func (param i32 i32) (result i32)))
  (func $f0 (type $t0) (param $p0 i32) (result i32)
    local.get $p0)
  (func $f1 (type $t0) (param $p0 i32) (result i32)
    local.get $p0
    i32.load offset=8 align=1)
  (func $f2 (type $t0) (param $p0 i32) (result i32)
    global.get $g0
    i32.const 4
    i32.add
    global.get $g0
    global.get $g0
    local.get $p0
    i32.const 4
    i32.add
    i32.add
    global.set $g0
    block $B0
      loop $L1
        memory.size
        global.get $g0
        i32.const 16
        i32.shr_u
        i32.gt_u
        br_if $B0
        i32.const 1
        memory.grow
        drop
        br $L1
      end
    end
    local.get $p0
    i32.store align=1)
  (func $f3 (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32) (local $l3 i32) (local $l4 i32)
    local.get $p0
    i32.const -4
    i32.add
    i32.load align=1
    local.set $l1
    local.get $l1
    call $f2
    local.set $l3
    i32.const 0
    local.set $l2
    block $B0
      loop $L1
        local.get $l2
        local.get $l1
        i32.ge_u
        br_if $B0
        local.get $l3
        local.get $l2
        i32.add
        local.get $p0
        local.get $l2
        i32.add
        i32.load align=1
        i32.store align=1
        local.get $l2
        i32.const 4
        i32.add
        local.set $l2
        br $L1
      end
    end
    local.get $l3)
  (func $f4 (type $t1) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l2 i32)
    local.get $p0
    i32.load align=1
    local.set $l2
    local.get $l2
    local.get $p0
    i32.add
    local.get $p1
    i32.store offset=4 align=1
    local.get $l2
    i32.const -4
    i32.add
    local.set $l2
    local.get $l2
    i32.eqz
    if $I0 (result i32)
      local.get $p0
      local.get $p0
      i32.load offset=4 align=1
      call_indirect (type $t0) $T0
    else
      local.get $p0
      local.get $l2
      i32.store align=1
      local.get $p0
    end)
  (func $f5 (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32)
    local.get $p0
    i32.load align=1
    i32.eqz
    local.get $p0
    i32.load offset=4 align=1
    i32.eqz
    i32.const -1
    i32.xor
    i32.and
    if $I0 (result i32)
      local.get $p0
      local.get $p0
      i32.load offset=4 align=1
      call_indirect (type $t0) $T0
      local.set $l1
      local.get $p0
      local.get $l1
      i32.store offset=8 align=1
      local.get $p0
      i32.const 1
      i32.store offset=4 align=1
      local.get $l1
    else
      local.get $p0
    end)
  (func $f6 (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32)
    i32.const 16
    call $f2
    local.set $l1
    local.get $l1
    i32.const 0
    i32.store align=1
    local.get $l1
    i32.const 0
    i32.store offset=4 align=1
    local.get $l1
    i32.const 1
    i32.store offset=8 align=1
    local.get $l1
    local.get $p0
    i32.store offset=12 align=1
    local.get $l1)
  (func $f7 (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32) (local $l2 i32) (local $l3 i32) (local $l4 i32) (local $l5 i32) (local $l6 i32) (local $l7 i32) (local $l8 i32) (local $l9 i32) (local $l10 i32) (local $l11 i32) (local $l12 i32) (local $l13 i32) (local $l14 i32) (local $l15 i32) (local $l16 i32) (local $l17 i32) (local $l18 i32) (local $l19 i32) (local $l20 i32) (local $l21 i32) (local $l22 i32) (local $l23 i32) (local $l24 i32) (local $l25 i32) (local $l26 i32) (local $l27 i32) (local $l28 i32) (local $l29 i32) (local $l30 i32) (local $l31 i32) (local $l32 i32) (local $l33 i32)
    local.get $p0
    call $f5
    local.set $l1
    local.get $l1
    i32.load offset=8 align=1
    i32.const 1
    i32.eq
    if $I0 (result i32)
      local.get $l1
      i32.load offset=12 align=1
      local.set $l2
      local.get $l2
      local.set $l3
      local.get $l3
      i32.const 1
      i32.eq
      if $I1 (result i32)
        i32.const 1
        call $f6
      else
        local.get $l3
        i32.const 0
        i32.eq
        if $I2 (result i32)
          i32.const 1
          call $f6
        else
          local.get $l1
          local.set $l5
          local.get $l5
          i32.load offset=8 align=1
          i32.const 1
          i32.eq
          if $I3 (result i32)
            local.get $l5
            i32.load offset=12 align=1
            local.set $l6
            i32.const 1
            call $f6
            local.set $l7
            local.get $l7
            i32.load offset=8 align=1
            i32.const 1
            i32.eq
            if $I4 (result i32)
              local.get $l7
              i32.load offset=12 align=1
              local.set $l8
              local.get $l6
              local.get $l8
              i32.sub
              local.set $l9
              local.get $l9
              call $f6
              call $f7
            else
              i32.const -1
              unreachable
            end
          else
            i32.const -1
            unreachable
          end
          local.set $l4
          local.get $l4
          i32.load offset=8 align=1
          i32.const 1
          i32.eq
          if $I5 (result i32)
            local.get $l4
            i32.load offset=12 align=1
            local.set $l10
            local.get $l10
            local.set $l12
            local.get $l12
            i32.load offset=8 align=1
            i32.const 1
            i32.eq
            if $I6 (result i32)
              local.get $l12
              i32.load offset=12 align=1
              local.set $l13
              i32.const 2
              call $f6
              local.set $l14
              local.get $l14
              i32.load offset=8 align=1
              i32.const 1
              i32.eq
              if $I7 (result i32)
                local.get $l14
                i32.load offset=12 align=1
                local.set $l15
                local.get $l13
                local.get $l15
                i32.sub
                local.set $l16
                local.get $l16
                call $f6
                call $f7
              else
                i32.const -1
                unreachable
              end
            else
              i32.const -1
              unreachable
            end
            local.set $l11
            local.get $l11
            i32.load offset=8 align=1
            i32.const 1
            i32.eq
            if $I8 (result i32)
              local.get $l11
              i32.load offset=12 align=1
              local.set $l17
              local.get $l10
              local.get $l17
              i32.add
              local.set $l18
              local.get $l18
              call $f6
            else
              i32.const -1
              unreachable
            end
          else
            i32.const -1
            unreachable
          end
        end
      end
    else
      local.get $l1
      local.set $l20
      local.get $l20
      i32.load offset=8 align=1
      i32.const 1
      i32.eq
      if $I9 (result i32)
        local.get $l20
        i32.load offset=12 align=1
        local.set $l21
        i32.const 1
        call $f6
        local.set $l22
        local.get $l22
        i32.load offset=8 align=1
        i32.const 1
        i32.eq
        if $I10 (result i32)
          local.get $l22
          i32.load offset=12 align=1
          local.set $l23
          local.get $l21
          local.get $l23
          i32.sub
          local.set $l24
          local.get $l24
          call $f6
          call $f7
        else
          i32.const -1
          unreachable
        end
      else
        i32.const -1
        unreachable
      end
      local.set $l19
      local.get $l19
      i32.load offset=8 align=1
      i32.const 1
      i32.eq
      if $I11 (result i32)
        local.get $l19
        i32.load offset=12 align=1
        local.set $l25
        local.get $l25
        local.set $l27
        local.get $l27
        i32.load offset=8 align=1
        i32.const 1
        i32.eq
        if $I12 (result i32)
          local.get $l27
          i32.load offset=12 align=1
          local.set $l28
          i32.const 2
          call $f6
          local.set $l29
          local.get $l29
          i32.load offset=8 align=1
          i32.const 1
          i32.eq
          if $I13 (result i32)
            local.get $l29
            i32.load offset=12 align=1
            local.set $l30
            local.get $l28
            local.get $l30
            i32.sub
            local.set $l31
            local.get $l31
            call $f6
            call $f7
          else
            i32.const -1
            unreachable
          end
        else
          i32.const -1
          unreachable
        end
        local.set $l26
        local.get $l26
        i32.load offset=8 align=1
        i32.const 1
        i32.eq
        if $I14 (result i32)
          local.get $l26
          i32.load offset=12 align=1
          local.set $l32
          local.get $l25
          local.get $l32
          i32.add
          local.set $l33
          local.get $l33
          call $f6
        else
          i32.const -1
          unreachable
        end
      else
        i32.const -1
        unreachable
      end
    end)
  (func $fibonacci (type $t0) (param $p0 i32) (result i32)
    (local $l1 i32)
    i32.const 0
    global.set $g0
    local.get $p0
    call $f6
    call $f7
    local.set $l1
    local.get $l1
    i32.load offset=8 align=1
    i32.const 1
    i32.eq
    if $I0 (result i32)
      local.get $l1
      i32.load offset=12 align=1
      local.set $l1
      local.get $l1
    else
      i32.const -1
      unreachable
    end)
  (func $f9 (type $t1) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l2 i32) (local $l3 i32) (local $l4 i32) (local $l5 i32) (local $l6 i32)
    local.get $p0
    call $f5
    local.set $l2
    local.get $l2
    i32.load offset=8 align=1
    i32.const 1
    i32.eq
    if $I0 (result i32)
      local.get $l2
      i32.load offset=12 align=1
      local.set $l3
      local.get $p1
      call $f5
      local.set $l4
      local.get $l4
      i32.load offset=8 align=1
      i32.const 1
      i32.eq
      if $I1 (result i32)
        local.get $l4
        i32.load offset=12 align=1
        local.set $l5
        local.get $l3
        local.get $l5
        i32.add
        local.set $l6
        local.get $l6
        call $f6
      else
        i32.const -1
        unreachable
      end
    else
      i32.const -1
      unreachable
    end)
  (func $f10 (type $t1) (param $p0 i32) (param $p1 i32) (result i32)
    (local $l2 i32) (local $l3 i32) (local $l4 i32) (local $l5 i32) (local $l6 i32)
    local.get $p0
    call $f5
    local.set $l2
    local.get $l2
    i32.load offset=8 align=1
    i32.const 1
    i32.eq
    if $I0 (result i32)
      local.get $l2
      i32.load offset=12 align=1
      local.set $l3
      local.get $p1
      call $f5
      local.set $l4
      local.get $l4
      i32.load offset=8 align=1
      i32.const 1
      i32.eq
      if $I1 (result i32)
        local.get $l4
        i32.load offset=12 align=1
        local.set $l5
        local.get $l3
        local.get $l5
        i32.sub
        local.set $l6
        local.get $l6
        call $f6
      else
        i32.const -1
        unreachable
      end
    else
      i32.const -1
      unreachable
    end)
  (table $T0 2 2 funcref)
  (memory $Memory 1)
  (global $g0 (mut i32) (i32.const 0))
  (export "Memory" (memory 0))
  (export "fibonacci" (func $fibonacci))
  (elem $e0 (i32.const 0) $f0 $f1))
