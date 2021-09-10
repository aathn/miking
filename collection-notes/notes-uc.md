# Unified Collections

## Properties

In this framework, collections are conceptually sequences
modulo some relations, which we call semantic properties.

    type Coll p a ~ [a] / p

In practice, we provide `Coll p a` as a base type. We might have a
declaration mechanism for properties such as below.

    prop NonSeq =      a ++ b ~ b ++ a
    prop UniqL  = a ++ b ++ a ~ a ++ b
    prop UniqR  = a ++ b ++ a ~ b ++ a
    prop Idem   =      a ++ a ~ a

We give a property map for each property, describing how to emulate the
given property using a sequence. For instance, a collection with unique
elements has a property map deleting duplicates, and a nonsequential
collection has a property map sorting the collection in some specific order.

    propmap NonSeq = sort
    propmap UniqL  = distinct
    propmap UniqR  = distinctRight
    propmap Idem   = distinctConsec

Each property map has the type

    p : all a. EqOrd a -> [a] -> [a],

where we define `EqOrd` as follows.

    type EqOrd a = {
      eq  : a -> a -> Bool
      cmp : a -> a -> Int
    }

Note that the main purpose of the property maps is to define the semantics
of collection operations, and they (and the `EqOrd`) will only be needed
computationally when checking that a concrete collection satisfies its
specification.

Depending on how (un-)useful we think it would be to define new properties, we
could also (1) refrain from providing declaration mechanisms such as
these, and just provide a hard-coded set of properties and maps, or
(2) only provide a declaration mechanism for property maps, leaving the
underlying property implicit.

We assume that all properties are composable unless stated otherwise,
i.e. their composite property maps can be computed by repeated
application of the base property maps.

We might have a directive to specify uncomposable properties:

    ## uncomposable UniqL, UniqR

This means that any property set containing both UniqL and UniqR
is invalid. We must check that the other properties are actually
composable (presumably by hand), but this saves us from having to give
an exponential number of property maps. Here, `##` is meant to
represent a compiler directive, since this is not something we
necessarily want as part of the object language.

## Defining Collection Operations

We provide three primitives for computing with collections:

    empty  : all p a. Coll p a
    insert : all p a. a -> Coll p a -> Coll p a
    fold   : all p a b. b -> (a -> b -> b) -> Coll p a -> b

We can write functions using these primitives, such as those below.

    let append : all p a. Coll p a -> Coll p a -> Coll p a
      = lam c1. lam c2. fold c2 insert c1

    let map : all p a b. (a -> b) -> Coll p a -> Coll p b
      = lam f. fold empty (compose insert f)

    let sum : all p. Coll p Int -> Int
      = fold 0 addi

    let find : all p a. (a -> Bool) -> Coll p a -> Option a
      = lam pred. lam c.
        fold (None ()) (lam h t. if pred h then Some h else t)

    let member : all p a. (a -> a -> Bool) -> a -> Coll p a -> Bool
      = lam eq. lam x.
        compose optionIsSome (find (eq x))

To accommodate maps, we may have a type

    type KVPair k v = { key : k, value : v },

and define functions

    let lookup : all p k v. (k -> k -> Bool) -> k -> Coll p (KVPair k v) -> Option v
      = lam eq k c.
        optionMap (lam x. x.value)
          (find (lam x. eq k x.key) c)

We may also allow pattern matching. This could be desugared to a
`fold`, provided the match form is restricted á la Coq for
collections (perhaps this is best to leave for later).

    let sum = lam c.
      match c with [] then
        0
      else match c with [a] ++ b then
        a + sum b
      else never

## Programming with Collections

To program using collections, we simply make use of the primitives
and/or previously defined operations.

    recursive let myFun : Int -> a -> Coll p a =
      lam n. lam x.
        if lti n 1 then
          insert x empty
        else
          let c = myFun (subi n 1) x in
          append c c

    let mySequence : Coll {}              = myFun 3 1
    let mySet      : Coll {NonSeq, UniqL} = myFun 3 1

    mexpr
    utest size mySequence with 8 in
    utest size mySet      with 1 in
    ()

Annotations specifying a collection's properties are required wherever
they cannot be inferred.

We are assuming that the element type can be queried for supported
operations, so that one does not have to explicitly give e.g. an
ordering when using a collection.

## Defining Representations

We can declare existing MCore data types as representations for
collections of various properties. We declare this on a per-function
basis.

For instance, suppose we want to declare the order-based set in the
standard library as a collection representation. We have

    let setEmpty : (a -> a -> Int) -> Set a = lam cmp. mapEmpty cmp
    let setSize : Set a -> Int = mapSize
    ...

We would declare as follows:

    ## repr Set for Coll {NonSeq, UniqL} where
      empty = setEmpty :: O(1) requiring Ord

    let setEmpty = ...

    ## repr Set for Coll {NonSeq, UniqL} where
      size = setSize :: O(n)

    let setSize = ...

    ...

I make use of `##` to represent compiler directives, since we do not
necessarily want this as part of the AST, I am guessing. For each
directive to be valid, the types of the functions should match. For
instance, we have

    empty    : Coll p a
    setEmpty : (a -> a -> Int) -> Set a,

where the types match, up to the difference in `Coll p` / `Set` and
the extra argument to `setEmpty`. This argument is explained by the
`requiring Ord` clause of the directive, specifying additional
operations on the element type. For now, this is simply represented as
arguments to the function, but a more sophisticated system could make
use of type classes.

The cost `f` inside the `O(f)` should be interpreted roughly as an
asymptotic cost in the size of the collection arguments. I propose we
let this be a function `f : Int -> Int` which is simply evaluated at
some large `n` when ordering the different operations.

We could assign several operations in one directive, and we could give
the directive separately from the implementation.

    ## repr Set for Coll {NonSeq, UniqL} where
      empty = setEmpty :: O(1) requiring Ord
      size  = setSize :: O(n)
      ...

We could also give several property sets for one operation. For
instance, a list may efficiently represent both a sequence and
an idempotent sequence:

    ## repr List for Coll {}, Coll {Idem} where empty = [] :: O(1)

These declarations are intended to be open, so that they do not have
to be specified all at once.

To specify a map representation, we give `KVPair` as the element type:

    ## repr Map k v for Coll {NonSeq, UniqL} (KVPair k v) where
      lookup = mapLookup :: O(log n) requiring Ord k
      ...

The same type parameters must appear in both left and right hand side.
Here, it is implicitly understood that `KVPair` should be ordered by
its keys. (I'm not quite sure how we would express this in practice,
using typeclasses seems like the most elegant option.)

## Selecting a Representation

When selecting a representation, we perform an analysis to find which
operations may be used on the collection in question. For instance,
for `mySet` above, we derive the set

    empty, insert, append, size

Why not `myFun`? Well, since no representation has declared to
implement `myFun`.

Making a declaration

    repr R for Coll p where op = opR :: O(f)

has two effects:

  1. It lets the compiler know that `R` implements `op`
  2. It lets the compiler know that `op` is replaceable

When making the analysis, we track the replaceable operations used on
the collection, and we consider only representations supporting all
the inferred operations. Thus, care must be taken that when one
representation provides an implementation for an operation, that all
other representations supporting that operation also do so (or those
representations will not be considered at all whenever that operation
is used). [the design here seems debatable]

We select the representation minimizing the maximal cost of any one
operation, and whose requirements are fulfilled by the element type.

For instance, in this case, we might consider `Set` from the standard
library as a possible representation. `Set` provides `empty` requiring
`Ord`, so to be able to select `Set` as a representation, the element
type (which is `Int` in this case) must be ordered (which it is).

The selection phase can be split into several steps:

  1. Compute the list of replaceable operations
  2. Analyse the program to find the set of replaceable operations
     used on each collection
  3. Find the set of possible representations for each collection
     (considering requirements placed on the element type and
     operations supported by the representation)
  4. Find the best representation for each collection

When I say "each collection", I really mean "each collection
expression not encapsulated by a let-bound function" throughout the
program. The output of our selection phase is a representation for
each such expression.

## Transforming the Program

Given a representation for each collection expression, we can
transform the program to use a dictionary-passing style.

For the previous example we might get something like the following
(where `mySequence` is skipped for brevity).

    let setDict = {
      empty  : setEmpty subi,
      insert : setInsert,
      append : setUnion,
      size   : setSize,
      ...
    }

    recursive let myFun =
      lam repr. lam n. lam x.
        if lti n 1 then
          repr.insert x repr.empty
        else
          let c = myFun repr (subi n 1) x in
          repr.append c c

    let mySet = myFun setDict 3 1

    mexpr
    utest setDict.size mySet with 1 in
    ()

Note that in general, we might need record subtyping to make this
resulting program typeable. This is a result of allowing functions
to be polymorphic in the operations of a collection in a way which
closely resembles subtype polymorphism. Perhaps we could escape this
with partial specialization?

For a reference on parametric overloading, see
http://okmij.org/ftp/Computation/typeclass.html.

## Testing Representation Correctness

We want to make sure that all representations behave as specified, so
that the semantics of our resulting program is consistent regardless of
the representation chosen.

For each declaration

    repr R for Coll p where op = opR :: O(...) requiring ...

we would like to check that `opR` really behaves as `op`. This can be
done automatically using property-based testing. There are two main
cases:

  1. `op` has a non-collection return type.
  2. `op` has a collection return type.

### Case 1

Here, we have

    op : arg1 -> arg2 -> ... -> argn -> T

for some `T` not equal to `Coll`. Then, we can test that

    op a1 a2 ... an = opR a1' a2' ... an'      (*)

for randomly generated arguments `a1, a1', ..., an, an'`. Whenever
`argi` has a collection type, we let `ai` be a random sequence of a
suitable ordered type, and

    ai' = foldr insertR emptyR ai,

where `foldr` is the MCore sequence fold. In other words, `ai'` is
`ai`, but in the representation `R`; this always works if we
mandate that all representations provide `empty` and `insert`, which
seems reasonable. For all other arguments `argj`, we let `aj = aj'` be
a suitable randomly generated argument of that type.

The left hand side of `(*)` is computed using the definition of `op`
in terms of the three primitives `empty`, `insert` and `fold`, with

    empty  = [],
    insert = cons,
    fold   = compose foldr (propmap p eqord),

Where `eqord : EqOrd a` is instantiated with the equality and order
for each element type `a` as appropriate. The right hand side is
computed using the definition of `opR`.

In this way, we can compare the two definitions and check that the
representation conforms to its specification.

### Case 2

Here, we have

    op : arg1 -> arg2 -> ... -> argn -> Coll p a.

We can proceed just like above, but with a slightly modified
correctness condition:

    foldr insertR emptyR (op a1 a2 ... an) = opR a1' a2' ... an'

Since `op` produces a sequence and `opR` produces an `R`, we need to
convert the sequence to an `R` before we can compare for equality.

### Caveats

In the section above, I said to use a "suitable ordered type". What do
we mean by this? Actually, the specific ordering used should not matter
in most cases. In particular, for an operation

    op : all p a. Coll p a -> T

where `a` is parametric, the specific choice of ordering cannot
matter, and we can use any old ordered type, such as integers for
instance.

If on the other hand the operation has a type

    op : all p. Coll p X -> T

for some specific type `X`, then we make use of that type. If `X` has
a meaningful / important ordering, then the programmer is in charge of
informing us of this (using the as-of-now magical pseudo-typeclass
system), and if not, then any order goes, and we can use a universal
ordering of some kind.

Concretely, take the example of `KVPair`s

    insert : all p k v. KVPair k v -> Coll p (KVPair k v) -> Coll p (KVPair k v)

Here, we make use of the specified ordering for `KVPair`, ordering by
`k`'s ordering. Since `k` is parametric, we use integers to represent
it, obtaining the relevant information to order the `KVPair`s.
