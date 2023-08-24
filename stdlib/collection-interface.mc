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

-- These should be used to give default implementations for all other operations.

let empty : all p. all a. Coll o p a
  = never

let append_op
  : all p1. all p2. all a
  .  Coll p1 a
  -> a
  -> Coll p2 a
  = never

let append : all p. all a. Coll p a -> a -> Coll p a = append_op

let prepend_op
  : all p1. all p2. all a
  .  a
  -> Coll p1 a
  -> Coll p2 a
  = never

let prepend : all p. all a. a -> Coll p a -> Coll p a = prepend_op

let foldl
  : all p. all a. all acc
  . (acc -> a -> acc)
  -> acc
  -> Coll o p a
  -> acc
  = never

let foldr
  : all p. all a. all acc
  . (acc -> a -> acc)
  -> acc
  -> Coll p a
  -> acc
  = never

--------------------------
-- Composite operations --
--------------------------

-- Property manipulation

let view
  : all p1. all p2. all a
  .  Coll p1 a
  -> Coll p2 a
  = never

-- Monoid

let singleton : all p. all a. a -> Coll p a = never

let concat_op
  : all p1. all p2. all p3. all a
  .  Coll p1 a
  -> Coll p2 a
  -> Coll p3 a
  = never

let concat
  : all p. all a
  .  Coll p a
  -> Coll p a
  -> Coll p a
  = concat_op

let into
  : all p1. all p2. all a
  .  Coll p1 a
  -> Coll p2 a
  -> Coll p1 a
  = concat_op

-- Foldable

let foldl1
  : all p. all a
  . (a -> a -> a)
  -> Coll p a
  -> a
  = never

let foldr1
  : all p. all a
  . (a -> a -> a)
  -> Coll p a
  -> a
  = never

-- Functor / applicative

let map_op
  : all p1. all p2. all a. all b
  . (a -> b)
  -> Coll p1 a
  -> Coll p2 b
  = never

let map
  : all p. all a. all b
  . (a -> b)
  -> Coll p a
  -> Coll p b
  = map_op

let map2_op
  : all p1. all p2. all p3. all a. all b. all c
  . (a -> b -> c)
  -> Coll p1 a
  -> Coll p2 b
  -> Coll p3 c
  = never

let map2
  : all p. all a. all b
  . (a -> b)
  -> Coll p a
  -> Coll p a
  -> Coll p b
  = map2_op

-- Monad

let join_op
  : all p1. all p2. all p3. all a
  .  Coll p1 (Coll p2 a)
  -> Coll p3 a
  = never

let join
  : all p. all a
  .  Coll p (Coll p a)
  -> Coll p a
  = join_op

let concatMap_op
  : all p1. all p2. all p3. all a. all b
  . (a -> Coll p2 b)
  -> Coll p1 a
  -> Coll p3 b
  = never

let concatMap
  : all p. all a. all b
  . (a -> Coll p b)
  -> Coll p a
  -> Coll p b
  = concatMap_op

-- Traversable

let mapAccumL_op
  : all p1. all p2. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p1 b
  -> (a, Coll p2 c)
  = never

let mapAccumL
  : all p. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p b
  -> (a, Coll p c)
  = mapAccumL_op

let mapAccumR_op
  : all p1. all p2. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p1 b
  -> (a, Coll p2 c)
  = never

let mapAccumR
  : all p. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p b
  -> (a, Coll p c)
  = mapAccumR_op

let iter : all p. all a. (a -> ()) -> Coll p a -> ()
  = never

-- Filtering and predicates

let filterMap_op
  : all p1. all p2. all a. all b
  . (a -> Option b)
  -> Coll p1 a
  -> Coll p2 b
  = never

let filterMap
  : all p. all a. all b
  . (a -> Option b)
  -> Coll p a
  -> Coll p b
  = filterMap_op

let filter_op
  : all p1. all p2. all a
  . (a -> Bool)
  -> Coll p1 a
  -> Coll p2 a
  = never

let filter
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Coll p a
  = filter_op

let any
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Bool
  = never

let every
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Bool
  = never

let findMap
  : all p. all a
  . (a -> Option b)
  -> Coll p a
  -> Option b
  = never

let find
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Option a
  = never

let member
  : all p. all a
  .  a
  -> Coll o p a
  -> Bool
  = never

-- Size

let size : all p. all a. Coll p a -> Int
  = never

let null : all p. all a. Coll p a -> Bool
  = never

-- Key-value operations

let lookup
  : all p. all k. all v
  .  k
  -> Coll p (k, v)
  -> Option v
  = never

let hasKey
  : all p. all k. all v
  .  k
  -> Coll p (k, v)
  -> Bool
  = never

let mapWithKey_op
  : all p1. all p2. all k. all a. all b
  . (k -> a -> b)
  -> Coll p1 (k, a)
  -> Coll p2 (k, b)
  = never

let mapWithKey
  : all p. all k. all a. all b
  . (k -> a -> b)
  -> Coll p (k, a)
  -> Coll p (k, b)
  = mapWithKey_op

let mapValues_op
  : all p1. all p2. all k. all a. all b
  . (a -> b)
  -> Coll p1 (k, a)
  -> Coll p2 (k, b)
  = never

let mapValues
  : all p. all k. all a. all b
  . (a -> b)
  -> Coll p (k, a)
  -> Coll p (k, b)
  = mapValues_op

let mapAccumLWithKey_op
  : all p1. all p2. all k. all a. all b. all c
  . (a -> k -> b -> (a, c))
  -> a
  -> Coll p1 (k, b)
  -> (a, Coll p2 (k, c))
  = never

let mapAccumLWithKey
  : all p. all k. all a. all b. all c
  . (a -> k -> b -> (a, c))
  -> a
  -> Coll p (k, b)
  -> (a, Coll p (k, c))
  = mapAccumLWithKey_op

let mapAccumLValues_op
  : all p1. all p2. all k. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p1 (k, b)
  -> (a, Coll p2 (k, c))
  = never

let mapAccumLValues
  : all p. all k. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p (k, b)
  -> (a, Coll p (k, c))
  = mapAccumLValues_op

let mapAccumRWithKey_op
  : all p1. all p2. all k. all a. all b. all c
  . (a -> k -> b -> (a, c))
  -> a
  -> Coll p1 (k, b)
  -> (a, Coll p2 (k, c))
  = never

let mapAccumRWithKey
  : all p. all k. all a. all b. all c
  . (a -> k -> b -> (a, c))
  -> a
  -> Coll p (k, b)
  -> (a, Coll p (k, c))
  = mapAccumRWithKey_op

let mapAccumRValues_op
  : all p1. all p2. all k. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p1 (k, b)
  -> (a, Coll p2 (k, c))
  = never

let mapAccumRValues
  : all p. all k. all a. all b. all c
  . (a -> b -> (a, c))
  -> a
  -> Coll p (k, b)
  -> (a, Coll p (k, c))
  = mapAccumRValues_op

-- Indexing and order

let reverse_op : all p1. all p2. all a. Coll p1 a -> Coll p2 a
  = never

let reverse : all p. all a. Coll o p a -> Coll o p a
  = reverse_op

let splitAt_op
  : all p1. all p2. all p3. all a
  .  Coll p1 a
  -> Int
  -> (Coll p2 a, Coll p3 a)
  = never

let splitAt
  : all p. all a
  .  Coll p a
  -> Int
  -> (Coll p a, Coll p a)
  = splitAt_op

let getAt
  : all p. all a
  .  Coll p a
  -> Int
  -> a
  = never

let setAt_op
  : all p1. all p2. all a
  .  Coll p1 a
  -> Int
  -> a
  -> Coll p2 a
  = never

let setAt
  : all p. all a
  .  Coll p a
  -> Int
  -> a
  -> Coll p a
  = set_op

let first : all p. all a. Coll p a -> a
  = never

let last : all p. all a. Coll p a -> a
  = never

let tail_op : all p. all a. Coll p a -> Coll p a
  = never

let tail : all p1. all p2. all a. Coll p1 a -> Coll p2 a
  = tail_op

let init_op : all p1. all p2. all a. Coll p1 a -> Coll p2 a
  = never

let init : all p. all a. Coll p a -> Coll p a
  = init_op

let mapi_op
  : all p1. all p2. all a. all b
  . (Int -> a -> b)
  -> Coll p1 a
  -> Coll p2 b
  = never

let mapi
  : all p. all a. all b
  . (Int -> a -> b)
  -> Coll p a
  -> Coll p b
  = mapi_op

let iteri_op
  : all p1. all p2. all a
  . (Int -> a -> ())
  -> Coll p1 a
  -> ()
  = never

let iteri
  : all p. all a
  . (Int -> a -> ())
  -> Coll p a
  -> ()
  = never

let create : all a. all p
  .  Int
  -> (Int -> a)
  -> Coll p a
  = never

let rangei_op : all a. all p1. all p2
  .  Coll p1 a
  -> Int
  -> Int
  -> Coll p2 a
  = never

let rangei : all a. all p.
  .  Coll p a
  -> Int
  -> Int
  -> Coll p a
  = rangei_op

-- Nonsequential operations

let add
  : all p. all a
  .  a
  -> Coll p a
  -> Coll p a
  = prepend
