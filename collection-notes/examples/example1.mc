recursive let myFun : Int -> a -> Coll p a =
  lam n. lam x.
    if lti n 1 then
      {x}
    else
      let c = myFun (subi n 1) x in
      append c c
end

let mySequence : Coll {}              = myFun 3 1
let mySet      : Coll {NonSeq, UniqL} = myFun 3 1

mexpr
utest size mySequence with 8 in
utest size mySet      with 1 in
()
