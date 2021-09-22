let append = lam c1. lam c2. fold insert c2 c1
let size = fold (lam. addi 1) 0

recursive let myFun : Int -> a -> Coll p a =
  lam n. lam x.
    if lti n 1 then
      insert x empty
    else
      let c = myFun (subi n 1) x in
      append c c
end

let mySequence : Coll {}       = myFun 3 1
let mySet      : Coll {UQ, NS} = myFun 3 1

mexpr
utest size mySequence with 8 in
utest size mySet      with 1 in
()
