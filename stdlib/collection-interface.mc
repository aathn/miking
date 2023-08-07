---------------------------
-- Collection properties --
---------------------------

type KeepAll
type KeepLast
type KeepLastKey

type InsertionOrder
type SortedOrder

----------------------------
-- Fundamental operations --
----------------------------

let empty : all o. all p. all a. Coll o p a
  = never

let insert
  : all o. all p. all a. a -> Coll o p a -> Coll o p a
  = never

let foldl
  : all o. all p. all a. all acc. (acc -> a -> acc) -> acc -> Coll o p a -> acc
  = never

--------------------------
-- Composite operations --
--------------------------

-- Collections are Foldable

let foldr
  : all o. all p. all a. all acc. (a -> acc -> acc) -> acc -> Coll o p a -> acc
  = never

let foldl1
  : all o. all p. all a. (a -> a -> a) -> Coll o p a -> a
  = never

let foldr1
  : all o. all p. all a. (a -> a -> a) -> Coll o p a -> a
  = never

let concat
  : all o1. all o2. all p1. all p2. all a. Coll o1 p1 a -> Coll o2 p2 a -> Coll o1 p1 a
  = never

let join
  : all o1. all o2. all p1. all p2. all a. Coll o1 p1 (Coll o2 p2 a) -> Coll o2 p2 a
  = never

let concatMap
  : all o1. all o2. all p1. all p2. all a. all b.
    (a -> Coll o2 p2 b)
      -> Coll o1 p1 a
      -> Coll o2 p2 b
  = never

let map
  : all o. all p. all a. all b.
    (a -> b)
      -> Coll o p a
      -> Coll o p b
  = never

let filter
  : all o. all p. all a. all b.
    (a -> Bool)
      -> Coll o p a
      -> Coll o p a
  = never

let size
  : all o. all p. all a. Coll o p a -> Int
  = never

let null : all o. all p. all a. Coll o p a -> Bool
  = never

-- Key-value operations
let lookup
  : all o. all p. all k. all v. k -> Coll o p (k, v) -> Option v
  = never

let mapWithKey
  : all o. all p. all k. all a. all b. (k -> a -> b)
  -> Coll o p (k, a)
  -> Coll o p (k, b)
  = never


-- let create : all a. Int -> (Int -> a) -> Seq a = never
-- let get : all a. Seq a -> Int -> a = never
-- let set : all a. Seq a -> Int -> a -> Seq a = never
-- let cons : all a. a -> Seq a -> Seq a = never
-- let snoc : all a. Seq a -> a -> Seq a = never
-- let splitAt : all a. Seq a -> Int -> (Seq a, Seq a) = never
-- let reverse : all a. Seq a -> Seq a  = never
-- let head : all a. Seq a -> a = never
-- let tail : all a. Seq a -> Seq a = never
-- let mapi : all a. all b. (Int -> a -> b) -> Seq a -> Seq b = never
-- let iter : all a. (a -> ()) -> Seq a -> () = never
-- let iteri : all a. (Int -> a -> ()) -> Seq a -> () = never
