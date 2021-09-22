let magicSort : [a] -> [a] = lam seq.
  let c : Coll {NS} = foldr insert empty seq in
  fold cons [] c

let magicDistinct : [a] -> [a] = lam seq.
  let c : Coll {UQ} = foldr insert empty seq in
  fold cons [] c

mexpr
utest magicSort     [2,2,1] with [1,2,2] in
utest magicDistinct [2,2,1] with [2,1]   in
()
