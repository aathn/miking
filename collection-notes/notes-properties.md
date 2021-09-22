# Side notes

These are some notes on the theory related to property maps. Unless
you are particularly interested, I suggest you skip them.

## On properties

In the context of having more properties than just `NS` and `UQ`, it
is of interest to know how to compute the property maps of composite
properties.

One notion of composability is, for two properties `P1` and `P2`, that
the fixpoint of the composite

    p1 . p2

coincides with the composite property map `p3`. This should be
a feasible check, but deducing `p3` may be nontrivial.

What would be better is if we could find a simpler condition to check,
in terms of the identities defining `P1` and `P2`.

What would be _even_ better is if we could view the identities as
rewrite rules, and automatically derive the property maps. Then, the
composability/commutativity condition would correspond to a confluence
condition: given a set of rewrite rules

    r1, r2, ..., rn,

the induced rewrite relation `r` should be confluent. But, deriving
property maps from the rewrite rules seems very difficult.

## On finite maps

Properties, as presented in my report, are relations induced by
universally quantified identities, such as

    aba ~ ba or ab ~ ba,

where variables are universally quantified and ranging over sequences.
In this presentation, a property map must essentially have the type

    p : all a. EqOrd a -> [a] -> [a].

At first glance, it seems that this prevents us from representing finite
maps, where we would like a property map of type

    p : all k v. EqOrd k -> [(k, v)] -> [(k, v)],

which would remove duplicates and sort only in the left components
(keys).

We can achieve a similar effect in the original proposal. Take `eqord`
as follows.

    let eqord : EqOrd (KVPair k v) =
      { eq  = lam x. lam y. eq x.key y.key
      , cmp = lam x. lam y. cmp x.key y.key
      }

Now, we can use a collection of `KVPairs` as a map, knowing that the
property map will only look at the keys.

There is a problem though, namely that `eq` and `cmp` now do not
mention the values whatsoever, so that we could have a "bad" property map
which, say, shuffles the values in addition to removing duplicate keys.
Such a property map would not capture the semantics of a finite map correctly.

In practice, if we use a stable `sort` and a `distinct` which does not
perform more work than needed, we do get the correct semantics. We
would like to refine our notion of property maps to _force_ these to
be the 'correct' maps for `NS` and `UQ`. Ideally, we could achieve
this without altering the original property equations.

As an intuitive idea, we could require that whenever there are several
admissible targets for a property map (i.e. the minimal element with
respect to the property relation is actually an equivalence class),
the property map must choose a target as "close" as possible to the
original sequence. For instance, for the property `UQ` with the
equivalence relation defined above, the sequence

    [('x', 1), ('y', 2) ('x', 3)]

would need to be sent to

    [('x', z), ('y', w)]

for some `z` and `w` in the original presentation. Introducing this
"closeness" constraint would force

    z = 1, w = 2.

to get a result as "close" as possible to the original list.
I have not made this notion rigorous as of yet (perhaps there
is some parametricity constraint that could work?), but I think it is
feasible to imagine that the theory could work out without extending
the notion of property itself.
