# Side notes

## On properties

In the main proposal, I said that we rely on properties being
composable unless explicitly stated otherwise.

To actually check if two properties `P1` and `P2` are composable, we
must check that the fixpoint of the composite

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

In the main proposal, I use properties as presented in my report, that
is as relations induced by universally quantified identities, such as

    aba ~ ba or ab ~ ba,

where variables are universally quantified and ranging over sequences.
In this presentation, a property map must essentially have the type

    p : all a. EqOrd a -> [a] -> [a].

At first glance, it seems that this prevents us from representing finite
maps, where we would like a property map of type

    p : all k v. EqOrd k -> [(k, v)] -> [(k, v)],

which would remove duplicates only in the left components (keys), and
perhaps order by the keys as well.

However, this can be achieved by using the EqOrd above. Taking

    let eqord =
      { eq  = lam p1. lam p2. eq p1.0 p2.0
      , cmp = lam p1. lam p2. cmp p1.0 p2.0
      }

we can simply use the regular uniqueness property to get uniqueness of keys,
and so on.

There is one slight problem, namely that `eq` and `cmp` now do not
mention the values whatsoever, so that we could have a "bad" property map
which, say, shuffles the values in addition to removing duplicate keys.
Such a property map would not capture the semantics of a finite map correctly.

However, note that a typical implementation of `distinct` or `sort`
will not do any more work than needed, and this leads to "correct" property
maps which capture the semantics of a finite map when used in this context.

Hence, if we slightly refine our notion of what a property map is, I think
we can avoid this issue without changing the rest of the conceptual framework.

In particular, we could require that whenever there are several admissible
targets for a property map (i.e. the minimal element with respect to the
property relation is actually an equivalence class), the property map must
choose a target as "close" as possible to the original sequence.

So for instance, for the property UniqL with the equivalence relation
defined above, the sequence

    [('x', 1), ('y', 2) ('x', 3)]

would need to be sent to

    [('x', z), ('y', w)]

for some z and w. Introducing this "closeness" constraint would force

    z = 1, w = 2.

I'm not quite sure of the details, and it is quite possible that this
notion of "closeness" is not satisfactory either. There may be some
parametricity constraint that could work.

In any case, I believe that it should be possible to adapt the
property maps as presented in my report to be just expressive enough
to formulate the various collections we might want, such as maps,
using this sort of technique.

This is all very abstract of course, and in practice we could just use
`distinct` and `sort`, knowing they behave as we want. But, it would
be nice to have some sort of theoretical justification as well.

