include "option.mc"
include "common.mc"

let append = lam c1. lam c2. fold insert c2 c1
let uncons =
  let step = lam h. lam r.
    Some (optionMapOr (h,empty) (lam t. (h, uncurry insert t)) r)
  in
  fold step (None ())
let head = lam c. optionMap (lam x. x.0) (uncons c)
let tail = lam c. optionMapOr empty (lam x. x.1) (uncons c)

type Tree a
con Leaf : () -> Tree a
con Node : (a, Tree a, Tree a) -> Tree a

let bfs : (a -> a -> Bool) -> a -> Tree a -> Bool
  = lam eq. lam x. lam t.
  recursive let work = lam queue.
    let h = head queue in
    match h with None () then
      false
    else match h with Some (Leaf ()) then
      work (tail queue)
    else match h with Some (Node (v, l, r)) then
      if eq v x then
        true
      else
        work (append (tail queue) (insert l (insert r empty)))
    else never
  in
  let initialQueue : Coll {} = insert t empty in
  work initialQueue

mexpr
let t1 = Node (1, Leaf (), Node (2, Leaf (), Leaf ())) in
let t2 = Node (1, Node (3, Leaf (), Leaf ()), Leaf ()) in
utest bfs eqi 2 t1 with true in
utest bfs eqi 2 t2 with false in
()
