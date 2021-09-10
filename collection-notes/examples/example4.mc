type Ord a = a -> a -> Bool

## repr Set for Coll {NonSeq, UniqL} where
  empty  = setEmpty :: O(1) requiring Ord
  insert = setInsert :: O(log n)
  fold   = lam z. lam f. compose (foldr f z) setToSeq :: O(n)
  remove = setRemove :: O(log n)
  append = setUnion :: O(log n)
  size   = setSize :: O(n)
  member = setMem :: O(log n)

