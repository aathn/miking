type UColl p x

type Coll p x = Repr (UColl p x)

type KeepAll
type InsertionOrder
type Seq a = Coll (KeepAll, InsertionOrder) a

type Rope a = [a]
type ConsList a
con Cons : all a. (a, ConsList a) -> ConsList a
con Nil : all a. () -> ConsList a

recursive let consMake = lam count. lam elem.
  if leqi count 0 then Nil () else
  Cons (elem, (consMake (subi count 1) elem))
end
let blubber = dprint (consMake)

let collFromSeq : all a. all props. [a] -> Coll props a = never
let create : all a. Int -> (Int -> a) -> Seq a = never
let createList : all a. Int -> (Int -> a) -> Seq a = never
let createRope : all a. Int -> (Int -> a) -> Seq a = never
let isList : all a. Seq a -> Bool = never
let isRope : all a. Seq a -> Bool = never
let length : all a. Seq a -> Int = never
let concat : all a. Seq a -> Seq a -> Seq a = never
let get : all a. Seq a -> Int -> a = never
let set : all a. Seq a -> Int -> a -> Seq a = never
let cons : all a. a -> Seq a -> Seq a = never
let snoc : all a. Seq a -> a -> Seq a = never
let splitAt : all a. Seq a -> Int -> (Seq a, Seq a) = never
let reverse : all a. Seq a -> Seq a  = never
let head : all a. Seq a -> a = never
let tail : all a. Seq a -> Seq a = never
let null : all a. Seq a -> Bool = never
let map : all a. all b. (a -> b) -> Seq a -> Seq b = never
let mapi : all a. all b. (Int -> a -> b) -> Seq a -> Seq b = never
let iter : all a. (a -> ()) -> Seq a -> () = never
let iteri : all a. (Int -> a -> ()) -> Seq a -> () = never
let foldl : all a. all acc. (acc -> a -> acc) -> acc -> Seq a -> acc = never
let foldr : all a. all acc. (a -> acc -> acc) -> acc -> Seq a -> acc = never
let subsequence : all a. Seq a -> Int -> Int -> Seq a = never

let print : Seq Char -> () = never
let printError : Seq Char -> () = never
let float2string : Float -> Seq Char = never
let string2float : Seq Char -> Float = never
let stringIsFloat : Seq Char -> Bool = never
let readLine : () -> Seq Char = never
let argv : Seq (Seq Char) = never
let readFile : Seq Char -> Seq Char = never
let writeFile : Seq Char -> Seq Char -> () = never
let fileExists : Seq Char -> Bool = never
let deleteFile : Seq Char -> () = never
let command : Seq Char -> Int = never
let error : all a. Seq Char -> a = never

let bootParserParseMExprString : (Bool,) -> Seq (Seq Char) -> (Seq Char) -> BootParseTree = never
let bootParserParseMCoreFile : (Bool, Bool, Seq (Seq Char), Bool, Bool, Bool) -> Seq (Seq Char) -> (Seq Char) -> BootParseTree = never
let bootParserGetString : BootParseTree -> Int -> (Seq Char) = never

let tensorCreateUninitInt : Seq Int -> Tensor[Int] = never
let tensorCreateUninitFloat : Seq Int -> Tensor[Float]= never
let tensorCreateCArrayInt : Seq Int -> (Seq Int -> Int) -> Tensor[Int]= never
let tensorCreateCArrayFloat : Seq Int -> (Seq Int -> Float) -> Tensor[Float]= never
let tensorCreateDense : all a. Seq Int -> (Seq Int -> a) -> Tensor[a] = never
let tensorGetExn : all a. Tensor[a] -> Seq Int -> a = never
let tensorSetExn : all a. Tensor[a] -> Seq Int -> a -> () = never
let tensorShape : all a. Tensor[a] -> Seq Int = never
let tensorReshapeExn : all a. Tensor[a] -> Seq Int -> Tensor[a] = never
let tensorSliceExn : all a. Tensor[a] -> Seq Int -> Tensor[a] = never
let tensor2string : all a. (a -> Seq Char) -> Tensor[a] -> Seq Char = never

let blubber = dprint collFromSeq
