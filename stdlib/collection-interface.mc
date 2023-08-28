-- This file gives a general interface for collection types.
--
-- In the comments below, we distinguish between a collection's
-- *insertion history* and its *elements*.  The insertion history
-- is the sequence of elements `x1, x2, ..., xn` that have been
-- inserted into a collection `c` since its creation, in the order
-- they were inserted, preserving duplicates. On the other hand,
-- the elements `y1, y2, ..., yn` are the observable contents
-- of `c`, which may be sorted or filtered in some way (e.g.,
-- to remove duplicates) depending on `c`'s properties.
--
-- For example, if `c = append (append 1 empty) 1` is a set, then,
-- its insertion history is `1, 1`, while its single element is `1`.

---------------------------
-- Collection properties --
---------------------------

-- The properties of a collection determine the semantics of operations working
-- over that collection.  The properties are given by two components:
-- a selection indicating which elements in the insertion history to keep, and
-- an ordering determining how those elements should be sorted.
--
-- Intuitively, we can think of the selection as a filtering function f and the
-- ordering as a permutation g; then the elements of a collection with these
-- properties will be `g (f xs)`, where `xs` is the collection's insertion history.

-- Selection properties.

-- KeepAll indicates to keep all values in the insertion history.
-- Seen as a filtering function, this is the identity.
type KeepAll

-- KeepLast indicates to only keep the last occurrence of duplicate values.
type KeepLast

-- KeepLastKey applies to collections of key-value pairs `(k, v)`, and indicates
-- to only keep the last occurrence of pairs with duplicate keys.
type KeepLastKey

-- Orderings.

-- SeqOrder arranges the values in the same order they had in the insertion history.
-- Seen as a permutation, this is the identity.
type SeqOrder

-- SortedOrder arranges the values in sorted order.
type SortedOrder

----------------------------
-- Fundamental operations --
----------------------------

-- These should be used to give default implementations for all other operations.

-- `empty` denotes an empty collection with any properties.
let empty : all p. all a. Coll o p a
  = never

-- `append_op c a` appends `a` to the insertion history of `c`.
let append_op
  : all p1. all p2. all a
  .  Coll p1 a
  -> a
  -> Coll p2 a
  = never

let append : all p. all a. Coll p a -> a -> Coll p a = append_op

-- `prepend_op a c` prepends `a` to the insertion history of `c`.
let prepend_op
  : all p1. all p2. all a
  .  a
  -> Coll p1 a
  -> Coll p2 a
  = never

let prepend : all p. all a. a -> Coll p a -> Coll p a = prepend_op

-- `foldl f acc c` gives `f (... (f (f acc x1) x2) ...) xn`, where
-- `x1, x2, ..., xn` are the elements of `c`.  As stated above,
-- the elements are given by permuting and filtering the insertion
-- history as described by `c`'s properties.
let foldl
  : all p. all a. all acc
  . (acc -> a -> acc)
  -> acc
  -> Coll o p a
  -> acc
  = never

-- `foldr f acc c` gives `f x1 (... (f xn-1 (f xn acc)) ...)`, where
-- `x1, x2, ..., xn` are the elements of `c`.
let foldr
  : all p. all a. all acc
  . (a -> acc -> acc)
  -> acc
  -> Coll p a
  -> acc
  = never

--------------------------
-- Composite operations --
--------------------------

-- Property manipulation

-- `view c` gives a collection whose insertion history is the same as `c`'s.
-- For example, even if `c` is a set, `view`ing `c` as a sequence will retain
-- any duplicates inserted into `c`.
let view
  : all p1. all p2. all a
  .  Coll p1 a
  -> Coll p2 a
  = never

-- Monoid

-- `singleton a` is a singleton collection with element `a`, with any properties.
let singleton : all p. all a. a -> Coll p a = never

-- `concat_op c1 c2` creates a new collection whose insertion history is the same as
-- the elements of `c1` followed by the elements of `c2`.
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

-- `foldl1 f c` behaves as `foldl f (first c) (tail c)`.
let foldl1
  : all p. all a
  . (a -> a -> a)
  -> Coll p a
  -> a
  = never

-- `foldr1 f c` behaves as `foldr f (last c) (init c)`.
let foldr1
  : all p. all a
  . (a -> a -> a)
  -> Coll p a
  -> a
  = never

-- Functor / applicative

-- `map_op f c` gives a new collection whose insertion history is
-- `f x1, f x2, ..., f xn`, where `x1, x2, ..., xn` are the elements of `c`.
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

-- `map2_op f c1 c2` gives a new collection whose insertion history is
-- `f xi yj` for j ∈ 1..m, i ∈ 1..n  where `x1, x2, ..., xn` are the
-- elements of `c1` and `y1, y2, ... yn` are the elements of `c2`.
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

-- `join_op c` gives a new collection whose insertion history is
-- the concatenation of the elements of the collections contained in `c`.
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

-- `concatMap_op f c` gives a new collection whose insertion history is
-- the concatenation of the elements of the collections obtained by
-- mapping `f` over the elements of `c`.
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

-- `mapAccumL_op f acc c` behaves as a simultaneous fold and map, returning a pair
-- equivalent to `(foldl f1 acc c, map f2 c)`, where `f1` and `f2` denote `f`'s
-- composition with the first and second tuple projections, respectively.
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

-- `mapAccumR_op` is analogous to `mapAccumL_op`, but performs a right fold.
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

-- `iter f c` calls `f` on each element of `c`, returning unit.
let iter : all p. all a. (a -> ()) -> Coll p a -> ()
  = never

-- Filtering and predicates

-- `filterMap_op f c` constructs a new collection whose insertion history is
-- given by mapping `f` over the elements of `c` and discarding `None ()`-values.
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

-- `filter_op f c` constructs a new collection whose insertion history is
-- given by those elements of `c` for which `f` returns `true`.
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

-- `any f c` returns `true` if `f` returns `true` for any element of `c`.
let any
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Bool
  = never

-- `every f c` returns `true` if `f` returns `true` for every element of `c`.
let every
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Bool
  = never

-- `findMap f c` returns `Some y` for the first element `x` of `c` such that
-- `f x = Some y`, or returns `None ()` if there is no such `x`.
let findMap
  : all p. all a
  . (a -> Option b)
  -> Coll p a
  -> Option b
  = never

-- `find f c` returns `x` for the first element `x` of `c` such that
-- `f x = true`, or returns `None ()` if there is no such `x`.
let find
  : all p. all a
  . (a -> Bool)
  -> Coll p a
  -> Option a
  = never

-- `member x c` returns `true` iff `x` is an element of `c`.
let member
  : all p. all a
  .  a
  -> Coll o p a
  -> Bool
  = never

-- `isSubset c1 c2` returns `true` iff every element of `c1` is an element of `c2`.
let isSubset
  : all p1. all p2. all a
  . Coll p1 a -> Coll p2 a -> Bool
  = never

-- Size

-- `size c` returns the number of elements of `c`.
let size : all p. all a. Coll p a -> Int
  = never

-- `null c` returns `true` iff `size c` is 0.
let null : all p. all a. Coll p a -> Bool
  = never

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

let create : all p. all a
  .  Int
  -> (Int -> a)
  -> Coll p a
  = never

let getRange_op : all p1. all p2. all a
  .  Coll p1 a
  -> Int
  -> Int
  -> Coll p2 a
  = never

let getRange : all p. all a.
  .  Coll p a
  -> Int
  -> Int
  -> Coll p a
  = getRange_op

let removeFirst_op : all p1. all p2. all a
  .  a
  -> Coll p1 a
  -> Coll p2 a
  = never

let removeFirst : all p. all a
  .  a
  -> Coll p a
  -> Coll p a
  = removeFirst

-- Key-value operations

let lookup
  : all p. all k. all v
  .  k
  -> Coll p (k, v)
  -> Option v
  = never

let removeKey_op
  : all p1. all p2. all k. all v
  .  k
  -> Coll p1 (k, v)
  -> Coll p2 (k, v)
  = never

let removeKey
  : all p. all k. all v
  .  k
  -> Coll p (k, v)
  -> Coll p (k, v)
  = removeKey_op

let hasKey
  : all p. all k. all v
  .  k
  -> Coll p (k, v)
  -> Bool
  = never

let getKeys
  : all p1. all p2. all k. all v
  .  Coll p1 (k, v)
  -> Coll p2 k
  = never

let getValues
  : all p1. all p2. all k. all v
  .  Coll p1 (k, v)
  -> Coll p2 v
  = never

let intersectKeysWith_op
  : all p1. all p2. all p3. all k. all a. all b. all c
  . (a -> b -> c)
  -> Coll p1 (k, a)
  -> Coll p2 (k, b)
  -> Coll p3 (k, c)
  = never

let intersectKeysWith
  : all p. all k. all a. all b. all c
  . (a -> b -> c)
  -> Coll p (k, a)
  -> Coll p (k, b)
  -> Coll p (k, c)
  = intersectKeysWith_op

let intersectKeys
  : all p. all k. all a. all b. all c
  .  Coll p (k, a)
  -> Coll p (k, b)
  -> Coll p (k, c)
  = lam c1. lam c2.
  intersectKeysWith (lam. lam a. a) c1 c2

let unionKeysWith_op
  : all p1. all p2. all p3. all k. all a. all b. all c
  .  Coll p1 (k, a)
  -> Coll p2 (k, b)
  -> Coll p3 (k, c)
  = never

let unionKeysWith
  : all p. all k. all a. all b. all c
  . (a -> b -> c)
  -> Coll p (k, a)
  -> Coll p (k, b)
  -> Coll p (k, c)
  = unionKeysWith_op

let unionKeys
  : all p. all k. all a. all b. all c
  .  Coll p (k, a)
  -> Coll p (k, b)
  -> Coll p (k, c)
  = lam c1. lam c2.
  unionKeysWith (lam. lam a. a) c1 c2

let differenceKeys_op
  : all p1. all p2. all p3. all k. all a. all b
  .  Coll p1 (k, a)
  -> Coll p2 (k, b)
  -> Coll p3 (k, a)
  = never

let differenceKeys
  : all p. all k. all a. all b
  .  Coll p (k, a)
  -> Coll p (k, b)
  -> Coll p (k, a)
  = differenceKeys_op

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

let filterValues_op
  : all p1. all p2. all k. all a. all b
  . (a -> Bool)
  -> Coll p1 (k, a)
  -> Coll p2 (k, a)
  = never

let filterValues
  : all p. all k. all a. all b
  . (a -> Bool)
  -> Coll p (k, a)
  -> Coll p (k, a)
  = never

-- Set operations

let remove
  : all p. all a
  .  a
  -> Coll p a
  -> Coll p a
  = removeFirst

let add
  : all p. all a
  .  a
  -> Coll p a
  -> Coll p a
  = prepend

let difference_op
  : all p1. all p2. all p3. all a
  .  Coll p1 a
  -> Coll p2 a
  -> Coll p3 a
  = never

let difference
  : all p. all a
  .  Coll p a
  -> Coll p a
  -> Coll p a
  = difference_op

let intersection_op
  : all p1. all p2. all p3. all a
  .  Coll p1 a
  -> Coll p2 a
  -> Coll p3 a
  = never

let intersection
  : all p. all a
  .  Coll p a
  -> Coll p a
  -> Coll p a
  = intersection_op

let union_op
  : all p1. all p2. all p3. all a
  .  Coll p1 a
  -> Coll p2 a
  -> Coll p3 a
  = never

let union
  : all p. all a
  .  Coll p a
  -> Coll p a
  -> Coll p a
  = union_op
