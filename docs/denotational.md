Denotational Semantics
----------------------

[[g]]: set of results

**minimal property**:
when you erase staging annotations from a term,
you can the same set of results,
assuming that staging succeeds.

tree of constraints,
with now goals simplified now,
and later goals simplified later.

simplification example:
(== x 5), (== y x)
if only y is relevant, can simplify to
(== y 5)
