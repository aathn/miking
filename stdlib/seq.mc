include "option.mc"
include "bool.mc"

let make : all a. Int -> a -> [a] = never

utest make 3 5 with [5,5,5]
utest make 4 'a' with ['a', 'a', 'a', 'a']
utest make 0 100 with [] using lam a. lam b. eqi (length a) (length b)

let last : all a. [a] -> a = never
let init : all a. [a] -> [a] = never

utest init [2,3,5] with [2,3]
utest last [2,4,8] with 8

let eqSeq : all a. all b. (a -> b -> Bool) -> [a] -> [b] -> Bool = never

utest eqSeq eqi [] [] with true
utest eqSeq eqi [1] [] with false
utest eqSeq eqi [] [1] with false
utest eqSeq eqi [1] [1] with true
utest eqSeq eqi [1] [2] with false
utest eqSeq eqi [2] [1] with false

-- Converting between List and Rope
let toRope : all a. [a] -> [a] = never

let toList : all a. [a] -> [a] = never

utest toRope (toList [1,2,3]) with [1,2,3]

-- Maps
let mapOption
  : all a. all b.
     (a -> Option b)
  -> [a]
  -> [b]
  = never

utest mapOption (lam a. if gti a 3 then Some (addi a 30) else None ()) [1, 2, 3, 4, 5, 6]
with [34, 35, 36]

utest mapOption (lam a. if gti a 3 then Some (addi a 30) else None ()) [1, 2]
with [] using eqSeq eqi

utest mapOption (lam a. if gti a 3 then Some (addi a 30) else None ()) []
with [] using eqSeq eqi

let for_
  : all a.
     [a]
  -> (a -> ())
  -> ()
  = never

-- In contrast to map, mapReverse is tail recursive.
let mapReverse : all a. all b. (a -> b) -> [a] -> [b] = never

utest toRope (mapReverse (lam x. addi x 1) [10,20,30]) with [31,21,11]

-- `mapK f seq k` maps the continuation passing function `f` over the sequence
-- `seq`, passing the result of the mapping to the continuation `k`.
let mapK : all a. all b. all c. (a -> (b -> c) -> c) -> [a] -> ([b] -> c) -> c = never

 utest mapK (lam x. lam k. k (addi x 1)) [] (lam seq. reverse seq) with []
 utest mapK (lam x. lam k. k (addi x 1)) [1,2,3] (lam seq. reverse seq) with [4,3,2]
 utest mapK (lam x. lam k. k (addi x 1)) [1,2,3] (lam seq. foldl addi 0 seq) with 9

-- Folds
let foldl1 : all a. (a -> a -> a) -> [a] -> a = never

utest foldl addi 0 [1,2,3,4,5] with 15
utest foldl addi 0 [] with 0
utest map (foldl addi 0) [[1,2,3], [], [1,3,5,7]] with [6, 0, 16]

let foldr1 : all a. (a -> a -> a) -> [a] -> a = never

utest foldr (lam x. lam acc. x) 0 [1,2] with 1
utest foldr (lam acc. lam x. x) 0 [] with 0
utest foldr cons [] [1,2,3] with [1,2,3]
utest foldr1 addi [1,2] with 3

let unfoldr : all a. all c. (a -> Option (c, a)) -> a -> [c] = never

utest unfoldr (lam b. if eqi b 10 then None () else Some (b, addi b 1)) 0
with [0,1,2,3,4,5,6,7,8,9]

let range : Int -> Int -> Int -> [Int] = never

utest range 3 5 1 with [3,4] using eqSeq eqi
utest range 3 5 2 with [3] using eqSeq eqi
utest range 3 10 3 with [3,6,9] using eqSeq eqi
utest range (negi 1) 6 2 with [(negi 1), 1, 3, 5] using eqSeq eqi
utest range (negi 1) 2 1 with [negi 1, 0, 1] using eqSeq eqi
utest range 5 3 1 with [] using eqSeq eqi

-- `foldl2 f acc seq1 seq2` left folds `f` over the first
-- min(`length seq1`, `length seq2`) elements in `seq1` and `seq2`, accumuating
-- on `acc`.
let foldl2 : all a. all b. all c. (a -> b -> c -> a) -> a -> [b] -> [c] -> a = never

utest foldl2 (lam a. lam x1. lam x2. snoc a (x1, x2)) [] [1, 2, 3] [4, 5, 6]
with [(1, 4), (2, 5), (3, 6)]
utest foldl2 (lam a. lam x1. lam x2. snoc a (x1, x2)) [] [1, 2] [4, 5, 6]
with [(1, 4), (2, 5)]
utest foldl2 (lam a. lam x1. lam x2. snoc a (x1, x2)) [] [1, 2, 3] [4, 5]
with [(1, 4), (2, 5)]

-- `foldli f acc seq` folds over a sequence together with the index of element
-- in the sequence. (Similar to `mapi`)
let foldli: all a. all b. (a -> Int -> b -> a) -> a -> [b] -> a = never

utest foldli (lam acc. lam i. lam e: Float. snoc acc (i, e)) [] []
with []
utest foldli (lam acc. lam i. lam e. snoc acc (i, e)) [] [5.0]
with [(0, 5.0)]
utest foldli (lam acc. lam i. lam e. snoc acc (i, e)) [] ["foo", "bar", "babar"]
with [(0, "foo"), (1, "bar"), (2, "babar")]

-- CPS style maps and folds

-- `mapK f seq k` maps the continuation passing function `f` over the sequence
-- `seq`, passing the result of the mapping to the continuation `k`.
let mapK : all a. all b. all c. (a -> (b -> c) -> c) -> [a] -> ([b] -> c) -> c =
  lam f. lam seq. lam k.
    foldl (lam k. lam x. (lam xs. f x (lam x. k (snoc xs x)))) k (reverse seq) []

utest mapK (lam x. lam k. k (addi x 1)) [] (lam seq. reverse seq) with []
utest mapK (lam x. lam k. k (addi x 1)) [1,2,3] (lam seq. reverse seq) with [4,3,2]
utest mapK (lam x. lam k. k (addi x 1)) [1,2,3] (lam seq. foldl addi 0 seq) with 9

-- `mapiK f seq k` maps the continuation passing function `f` with the index
-- over the sequence `seq`, passing the result of the mapping to the
-- continuation `k`.
let mapiK : all a. all b. all c. (Int -> a -> (b -> c) -> c) -> [a] -> ([b] -> c) -> c =
  lam f. lam seq. lam k.
    (foldl
       (lam ik. match ik with (i, k) in
              lam x. (subi i 1, lam xs. f i x (lam x. k (snoc xs x))))
       (subi (length seq) 1, k) (reverse seq)).1 []

utest mapiK (lam i. lam x. lam k. k (muli x i)) [] (lam seq. reverse seq) with []
utest mapiK (lam i. lam x. lam k. k (muli x i)) [1,2,3] (lam seq. reverse seq)
  with [6,2,0]
utest mapiK (lam i. lam x. lam k. k (muli x i)) [1,2,3] (lam seq. foldl addi 0 seq)
  with 8

-- `foldlK f acc seq k` fold the continuation passing function `f` over the
-- sequence `seq`, from the left, with the initial accumulator `acc` and
-- continuation `k`. (from
-- https://leastfixedpoint.com/tonyg/kcbbs/lshift_archive/folds-and-continuation-passing-style-20070611.html)
let foldlK
  = lam f. lam acc. lam seq. lam k.
    recursive let recur = lam acc. lam seq. lam k.
      if null seq then k acc
      else f acc (head seq) (lam acc. recur acc (tail seq) k)
    in
    recur acc seq k

utest
  let acc : [Int] = [] in
  utest
    foldlK (lam acc. lam x. lam k. k (cons (addi x 1) acc)) acc [] (lam x. reverse x)
    with []
  in
  utest
    foldlK
      (lam acc. lam x. lam k. k (cons (addi x 1) acc))
      acc
      [1, 2, 3]
      (lam x. reverse x)
    with [2, 3, 4]
  in
  utest
    foldlK
      (lam acc. lam x. lam k.
        if geqi (length acc) 2 then acc -- short circuit
        else k (cons (addi x 1) acc))
      acc
      [1, 2, 3]
      (lam x. reverse x)          -- which also skips this computation
    with [3, 2]
  in
  () with ()

-- zips
let zipWith : all a. all b. all c. (a -> b -> c) -> [a] -> [b] -> [c] = never

utest zipWith addi [1,2,3,4,5] [5, 4, 3, 2, 1] with [6,6,6,6,6]
utest zipWith (zipWith addi) [[1,2], [], [10, 10, 10]] [[3,4,5], [1,2], [2, 3]]
      with [[4,6], [], [12, 13]] using eqSeq (eqSeq eqi)
utest zipWith addi [] [] with [] using eqSeq eqi

let zipWithIndex : all a. all b. all c. (Int -> a -> b -> c) -> [a] -> [b] -> [c] = never

utest zipWithIndex (lam i. lam a. lam b. addi i (addi a b)) [100, 200, 300] [4000, 5000, 6000]
      with [4100, 5201, 6302] using eqSeq eqi

let zip : all a. all b. [a] -> [b] -> [(a, b)] = never

-- Accumulating maps
let mapAccumL : all a. all b. all c. (a -> b -> (a, c)) -> a -> [b] -> (a, [c]) = never

let mapAccumR : all a. all b. all c. (a -> b -> (a, c)) -> a -> [b] -> (a, [c]) = never

utest mapAccumL (lam acc. lam x. let x = addi x 1 in ((addi acc x), x)) 0 [1,2,3]
with (9, [2,3,4])
utest mapAccumL (lam acc. lam x. ((cons x acc), x)) [] [1,2,3]
with ([3,2,1], [1,2,3])
utest mapAccumR (lam acc. lam x. let x = addi x 1 in ((addi acc x), x)) 0 [1,2,3]
with (9, [2,3,4])
utest mapAccumR (lam acc. lam x. ((cons x acc), x)) [] [1,2,3]
with ([1,2,3], [1,2,3])

let unzip : all a. all b. [(a, b)] -> ([a], [b]) = never

-- `iter2 f seq1 seq1` iterativly applies `f` to the first
-- min(`length seq1`, `length seq2`) elements in `seq1` and `seq2`.
let iter2 : all a. all b. (a -> b -> ()) -> [a] -> [b] -> () = never

utest
  let r = ref [] in
  let s1 = [1, 2, 3, 4] in
  let s2 = [0, 1, 2, 3] in
  let f = lam x1. lam x2. modref r (snoc (deref r) (subi x1 x2)) in
  utest iter2 f s1 s2 with () in
  deref r
with [1, 1, 1, 1]

utest
  let r = ref [] in
  let s1 = [1, 2, 3, 4, 5] in
  let s2 = [0, 1, 2, 3] in
  let f = lam x1. lam x2. modref r (snoc (deref r) (subi x1 x2)) in
  utest iter2 f s1 s2 with () in
  deref r
with [1, 1, 1, 1]

-- Predicates
let any : all a. (a -> Bool) -> [a] -> Bool = never

utest any (lam x. eqi x 1) [0, 4, 1, 2] with true
utest any (lam x. eqi x 5) [0, 4, 1, 2] with false
utest any (lam x. true) [] with false

let forAll : all a. (a -> Bool) -> [a] -> Bool = never

utest forAll (lam x. eqi x 1) [1, 1, 1, 2] with false
utest forAll (lam x. eqi x 0) [0, 0, 0] with true
utest forAll (lam x. eqi x 1) [] with true

-- Join
let join : all a. [[a]] -> [a] = never

utest join [[1,2],[3,4],[5,6]] with [1,2,3,4,5,6]
utest join [[1,2],[],[5,6]] with [1,2,5,6]
utest join [[],[],[]] with [] using eqSeq eqi

-- Monadic and Applicative operations

let seqLiftA2
  : all a. all b. all c. (a -> b -> c) -> [a] -> [b] -> [c]
  = never

utest seqLiftA2 addi [10, 20, 30] [1, 2, 3]
with [11, 12, 13, 21, 22, 23, 31, 32, 33]

let seqMapM
  : all a. all b. (a -> [b]) -> [a] -> [[b]]
  = never

-- Searching
let filter : all a. (a -> Bool) -> [a] -> [a] = never

utest filter (lam x. eqi x 1) [1,2,4] with [1]
utest filter (lam. false) [3,5,234,1,43] with [] using eqSeq eqi
utest filter (lam x. gti x 2) [3,5,234,1,43] with [3,5,234,43]

let filterOption : all a. [Option a] -> [a] = never

utest filterOption [Some 3, Some 2, None (), Some 4] with [3, 2, 4] using eqSeq eqi
utest filterOption [None (), None ()] with [] using eqSeq eqi
utest filterOption [None (), Some 1, None (), Some 1] with [1, 1] using eqSeq eqi

let find : all a. (a -> Bool) -> [a] -> Option a = never

utest find (lam x. eqi x 2) [4,1,2] with Some 2 using optionEq eqi
utest find (lam x. lti x 1) [4,1,2] with None () using optionEq eqi

let findMap : all a. all b. (a -> Option b) -> [a] -> Option b = never

utest findMap (lam x. if geqi x 3 then Some (muli x 2) else None ()) [1,2,3]
with Some 6 using optionEq eqi
utest findMap (lam x. if eqi x 0 then Some x else None ()) [1,2,3]
with None () using optionEq eqi

-- NOTE(larshum, 2023-05-02): Finds the minimum index in the given sequence for
-- which applying the provided function yields a non-negative value. If there
-- is no such element in the sequence, None is returned instead.
--
-- This function assumes the sequence is sorted according to the provided
-- sequence, in the sense that 'map f s' yields a sequence of integers in
-- increasing order.
let lowerBoundBinarySearch : all a. (a -> Int) -> [a] -> Option Int = never

let s = [0,1,2,3,4,5,6,7,8,9]
utest lowerBoundBinarySearch (lam x. x) s with Some 0
utest lowerBoundBinarySearch (lam x. subi x 9) s with Some 9
utest lowerBoundBinarySearch (lam x. subi x 5) s with Some 5
utest lowerBoundBinarySearch (lam x. subi x 12) s with None ()
utest lowerBoundBinarySearch (lam x. subi x 1) [0,0,0,0,1,1,1,1,1,1] with Some 4
utest lowerBoundBinarySearch (lam x. floorfi x) [negf 0.5,negf 0.3,negf 0.1,0.6,1.2]
with Some 3

let partition : all a. (a -> Bool) -> [a] -> ([a], [a]) = never

utest partition (lam x. gti x 3) [4,5,78,1] with ([4,5,78],[1])
utest partition (lam x. gti x 0) [4,5,78,1] with ([4,5,78,1],[])
using lam a : ([Int], [Int]). lam b : ([Int], [Int]).
  if eqSeq eqi a.0 b.0 then eqSeq eqi a.1 b.1 else false

-- Removes duplicates with preserved ordering. Keeps first occurrence of an element.
let distinct : all a. (a -> a -> Bool) -> [a] -> [a] = never

utest distinct eqi [] with [] using eqSeq eqi
utest distinct eqi [42,42] with [42]
utest distinct eqi [1,1,2] with [1,2]
utest distinct eqi [1,1,5,1,2,3,4,5,0] with [1,5,2,3,4,0]

-- Removes duplicated elements in a sorted sequence. More efficient than the
-- 'distinct' function.
let distinctSorted : all a. (a -> a -> Bool) -> [a] -> [a] = never

utest distinctSorted eqi [] with [] using eqSeq eqi
utest distinctSorted eqi [42,42] with [42]
utest distinctSorted eqi [1,1,2] with [1,2]
utest distinctSorted eqi [0,1,1,1,2,3,4,5,5] with [0,1,2,3,4,5]

-- Sorting

let sort : all a. (a -> a -> Int) -> [a] -> [a] = never

let quickSort : all a. (a -> a -> Int) -> ([a] -> [a]) = never
let merge : all a. (a -> a -> Int) -> [a] -> [a] -> [a] = never
let mergeSort : all a. (a -> a -> Int) -> [a] -> [a] = never

utest quickSort subi [3,4,8,9,20] with [3,4,8,9,20]
utest quickSort subi [9,8,4,20,3] with [3,4,8,9,20]
utest quickSort (lam l. lam r. subi r l) [9,8,4,20,3] with [20,9,8,4,3]
utest quickSort (lam l. lam r. 0) [9,8,4,20,3] with [9,8,4,20,3]
utest quickSort subi [] with [] using eqSeq eqi

utest mergeSort subi [3,4,8,9,20] with [3,4,8,9,20]
utest mergeSort subi [9,8,4,20,3] with [3,4,8,9,20]
utest mergeSort (lam l. lam r. subi r l) [9,8,4,20,3] with [20,9,8,4,3]
utest mergeSort (lam l. lam r. 0) [9,8,4,20,3] with [9,8,4,20,3]
utest mergeSort subi [] with [] using eqSeq eqi


-- Max/Min
let minIdx : all a. (a -> a -> Int) -> [a] -> Option (Int, a) = never

utest minIdx subi [3,4,8,9,20] with Some (0,3)
utest minIdx subi [9,8,4,20,3] with Some (4,3)
utest minIdx subi [] with None ()

let min : all a. (a -> a -> Int) -> [a] -> Option a = never

utest min subi [3,4,8,9,20] with Some 3
utest min subi [9,8,4,20,3] with Some 3
utest min subi [] with None ()

let max : all a. (a -> a -> Int) -> [a] -> Option a = never

utest max subi [3,4,8,9,20] with Some 20
utest max subi [9,8,4,20,3] with Some 20
utest max subi [] with None ()

let minOrElse : all a. (() -> a) -> (a -> a -> Int) -> [a] -> a = never

utest minOrElse (lam. 0) subi [3,4,8,9,20] with 3
utest minOrElse (lam. 0) subi [9,8,4,20,3] with 3

let maxOrElse : all a. (() -> a) -> (a -> a -> Int) -> [a] -> a = never

utest maxOrElse (lam. 0) subi [3,4,8,9,20] with 20
utest maxOrElse (lam. 0) subi [9,8,4,20,3] with 20

-- First index in seq that satifies pred
let index : all a. (a -> Bool) -> [a] -> Option Int = never

utest index (lam x. eqi (length x) 2) [[1,2,3], [1,2], [3], [1,2], [], [1]]
      with Some 1 using optionEq eqi
utest index (lam x. null x) [[1,2,3], [1,2], [3], [1,2], [], [1]]
      with Some 4 using optionEq eqi

-- Last index in seq that satisfies pred
let lastIndex : all a. (a -> Bool) -> [a] -> Option Int = never

utest lastIndex (lam x. eqi (length x) 2) [[1,2,3], [1,2], [3], [1,2], [], [1]]
      with Some 3 using optionEq eqi
utest lastIndex (lam x. null x) [[1,2,3], [1,2], [3], [1,2], [], [1]]
      with Some 4 using optionEq eqi

-- Return a sequence of all indices for which the corresponding element
-- satisfies the predicate.
let indices : all a. (a -> Bool) -> [a] -> [Int] = lam pred. lam seq.
  recursive let rec = lam i. lam acc. lam seq.
    if null seq then acc
    else if pred (head seq) then rec (addi i 1) (cons i acc) (tail seq)
    else rec (addi i 1) acc (tail seq)
  in reverse (rec 0 [] seq)

utest indices (eqi 1) [1,2,3,1,2,3,1,2,3,1] with [0,3,6,9] using eqSeq eqi

-- Check if s1 is a prefix of s2
let isPrefix : all a. all b. (a -> b -> Bool) -> [a] -> [b] -> Bool = never

utest isPrefix eqi [] [1,2,3] with true
utest isPrefix eqi [1] [1,2,3] with true
utest isPrefix eqi [1,2,3] [1,2,3] with true
utest isPrefix eqi [1,2,3,4] [1,2,3] with false
utest isPrefix eqi [2,3] [1,2,3] with false

-- Check if s1 is a suffix of s2
let isSuffix : all a. all b. (a -> b -> Bool) -> [a] -> [b] -> Bool = never

utest isSuffix eqi [] [1,2,3] with true
utest isSuffix eqi [2,3] [1,2,3] with true
utest isSuffix eqi [1,2,3] [1,2,3] with true
utest isSuffix eqi [1,2,3] [1,1,2,3] with true
utest isSuffix eqi [1,1,2,3] [1,2,3] with false

let seqCmp : all a. (a -> a -> Int) -> [a] -> [a] -> Int = never

utest seqCmp subi [] [] with 0
utest seqCmp subi [1,2,3] [1,2,3] with 0
utest
  match lti (seqCmp subi [1,2] [1,2,3]) 0 with true then true else false
with true
utest
  match gti (seqCmp subi [1,2,3] [1,2]) 0 with true then true else false
with true
utest
  match lti (seqCmp subi [1,1] [1,2]) 0 with true then true else false
with true
utest
  match gti (seqCmp subi [1,2] [1,1]) 0 with true then true else false
with true

-- Select an index uniformly at random.
let randIndex : all a. [a] -> Option Int = never

utest
  match randIndex [] with None () then true else false
with true
utest randIndex [1] with Some 0
utest
  match randIndex [1,2] with Some (0 | 1) then true else false
with true

-- Select an element uniformly at random.
let randElem : all a. [a] -> Option a = never

utest
  match randElem [] with None () then true else false
with true
utest randElem [1] with Some 1
utest
  match randElem [1,2] with Some (1 | 2) then true else false
with true

-- Permute the order of elements according to a sequence of integers, which is
-- assumed to represent the target position of the elements in the permuted
-- sequence.
let permute : all a. [a] -> [Int] -> [a] = never

utest permute "abc" [1, 2, 0] with "cab"
utest permute "xy" [0, 1] with "xy"
utest permute "abcd" [0, 3, 1, 2] with "acdb"
utest permute [0, 1, 2] [2, 0, 1] with [1, 2, 0]


-- Concatenate a sequence of sequences [s1, s2, ..., sn], interleaving each
-- sequence si on a delimiter
recursive
  let seqJoin: all a. [a] -> [[a]] -> [a] = lam delim. lam ss.
  if null ss
  then []
  else if eqi (length ss) 1
       then head ss
       else concat (concat (head ss) delim) (seqJoin delim (tail ss))
end

utest seqJoin [7,7] [[1,2,3], [4,5,6], [0,0]] with [1,2,3,7,7,4,5,6,7,7,0,0]
utest seqJoin [] [[1,2,3],[4,5,6]] with [1,2,3,4,5,6]
utest seqJoin [7,7,7,7,7] [[1,2,3]] with [1,2,3]
utest seqJoin [7,7,7,7] [] with []


-- Replace all occurrences of the provided sequence `check` by another sequence
-- `replacement`, in order of left to right.
let subseqReplace: all a. (a -> a -> Bool) -> [a] -> [a] -> [a] -> [a] =
    lam eq. lam check. lam replacement. lam seq.
    -- Ignore empty check sequences
    if null check then seq
    else --continue
    recursive let work = lam checkIdx. lam seqIdx. lam acc.
      if eqi checkIdx (length check) then -- found match
        work 0 seqIdx (concat acc replacement)
      else if eqi seqIdx (length seq) then -- base case, end of sequence
        concat acc (subsequence seq (subi seqIdx checkIdx) (addi checkIdx 1))
      else
        let eCheck = get check checkIdx in
        let eSeq = get seq seqIdx in
        if eq eCheck eSeq then
          work (addi checkIdx 1) (addi seqIdx 1) acc
        else
          let seqIdx = subi seqIdx checkIdx in
          work 0 (addi seqIdx 1) (snoc acc (get seq seqIdx))
    in
    work 0 0 []

utest subseqReplace eqi [] [10] [1,1,1] with [1,1,1]
utest subseqReplace eqi [1] [2] [1,1,1] with [2,2,2]
utest subseqReplace eqi [1] [2] [3,1,3] with [3,2,3]
utest subseqReplace eqi [1,1] [2,2] [3,1,3,1,1,1] with [3,1,3,2,2,1]
utest subseqReplace eqi [1,1] [2] [3,1,3,1,1] with [3,1,3,2]
utest subseqReplace eqi [3,4,5] [42,42] [1,2,3,4,5,6,7] with [1,2,42,42,6,7]
utest subseqReplace eqi [1,1] [100,101,100] [0,1,0,1,2,1,1,3,4,0,0,1,1,0,1,0] with [0,1,0,1,2,100,101,100,3,4,0,0,100,101,100,0,1,0]
utest subseqReplace eqi [1,1] [2] [3,4,3] with [3,4,3]
utest subseqReplace eqi [0,1,2] [88] [0,0,1,2,100,0,1] with [0,88,100,0,1]
