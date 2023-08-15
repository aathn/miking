---------------------------
-- Collection properties --
---------------------------

type KeepAll
type KeepLast
type KeepLastKey

type SeqOrder
type SortedOrder

----------------------------
-- Fundamental operations --
----------------------------

let empty : all o. all p. all a. Coll o p a
  = never

let append
  : all o. all p. all a
  .  Coll o p a
  -> a
  -> Coll o p a
  = never

let foldl
  : all o. all p. all a. all acc
  . (acc -> a -> acc)
  -> acc
  -> Coll o p a
  -> acc
  = never

--------------------------
-- Composite operations --
--------------------------

-- Property manipulation

let view
  : all o1. all p1. all o2. all p2. all a
  .  Coll o1 p1 a
  -> Coll o2 p2 a
  = never

let into
  : all o1. all p1. all o2. all p2. all a
  .  Coll o1 p1 a
  -> Coll o2 p2 a
  -> Coll o1 p1 a
  = never

-- Foldable

let foldr
  : all o. all p. all a. all acc
  . (a -> acc -> acc)
  -> acc
  -> Coll o p a
  -> acc
  = never

let foldl1
  : all o. all p. all a
  . (a -> a -> a)
  -> Coll o p a
  -> a
  = never

let foldr1
  : all o. all p. all a
  . (a -> a -> a)
  -> Coll o p a
  -> a
  = never

-- Functor / applicative

let map
  : all o. all p. all a. all b
  . (a -> b)
  -> Coll o p a
  -> Coll o p b
  = never

let map2
  : all o. all p. all a. all b. all c
  . (a -> b -> c)
  -> Coll o p a
  -> Coll o p b
  -> Coll o p c
  = never

-- Monoid / monad

let singleton : all o. all p. all a. a -> Coll o p a = never

let concat
  : all o. all p. all a
  .  Coll o p a
  -> Coll o p a
  -> Coll o p a
  = never

let join
  : all o. all p. all a
  .  Coll o p (Coll o p a)
  -> Coll o p a
  = never

let concatMap
  : all o. all p. all a. all b
  . (a -> Coll o p b)
  -> Coll o p a
  -> Coll o p b
  = never

-- Traversable

let mapAccumL
  : all o. all p. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll o p b
  -> (a, Coll o p c)
  = never

let mapAccumR
  : all o. all p. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll o p b
  -> (a, Coll o p c)
  = never

-- Filtering and predicates

let filterMap
  : all o. all p. all a. all b
  . (a -> Option b)
  -> Coll o p a
  -> Coll o p b
  = never

let filter
  : all o. all p. all a
  . (a -> Bool)
  -> Coll o p a
  -> Coll o p a
  = never

let any
  : all o. all p. all a
  . (a -> Bool)
  -> Coll o p a
  -> Bool
  = never

let every
  : all o. all p. all a
  . (a -> Bool)
  -> Coll o p a
  -> Bool
  = never

let findMap
  : all o. all p. all a
  . (a -> Option b)
  -> Coll o p a
  -> Option b
  = never

let find
  : all o. all p. all a
  . (a -> Bool)
  -> Coll o p a
  -> Option a
  = never

let member
  : all o. all p. all a
  .  a
  -> Coll o p a
  -> Bool
  = never

-- Size

let size : all o. all p. all a. Coll o p a -> Int
  = never

let null : all o. all p. all a. Coll o p a -> Bool
  = never

-- Key-value operations

let lookup
  : all o. all p. all k. all v
  .  k
  -> Coll o p (k, v)
  -> Option v
  = never

let hasKey
  : all o. all p. all k. all v
  .  k
  -> Coll o p (k, v)
  -> Bool
  = never

let mapWithKey
  : all o. all p. all k. all a. all b
  . (k -> a -> b)
  -> Coll o p (k, a)
  -> Coll o p (k, b)
  = never

let mapAccumLWithKey
  : all o. all p. all k. all a. all b. all c
  . (a -> k -> b -> (a, c))
  -> a
  -> Coll o p (k, b)
  -> (a, Coll o p (k, c))
  = never

let mapAccumRWithKey
  : all o. all p. all k. all a. all b. all c
  . (a -> k -> b -> (a, c))
  -> a
  -> Coll o p (k, b)
  -> (a, Coll o p (k, c))
  = never

-- Indexing and order

let reverse : all o. all p. all a. Coll o p a -> Coll o p a
  = never

let splitAt : all o. all p. all a. Coll o p a -> Int -> (Coll o p a, Coll o p a)
  = never

let first : all o. all p. all a. Coll o p a -> a
  = never

let last : all o. all p. all a. Coll o p a -> a
  = never

let tail : all o. all p. all a. Coll o p a -> Coll o p a
  = never

let init : all o. all p. all a. Coll o p a -> Coll o p a
  = never

let prepend : all o. all p. all a. a -> Coll o p a -> Coll o p a
  = never

-- Nonsequential operations

let add
  : all o. all p. all a
  .  a
  -> Coll o p a
  -> Coll o p a
  = never
