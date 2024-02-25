# algorithms

A place to explore various Scala 3 algorithmic adventures

initial "minimal duad" code
idea is to validate the equivalence of Cocantorian[H] with Duad[H]. Explicitly:
for a "Hausdorff" type H with computable equality == (and we'll take your word on this),
  CoCantorian[H] = Cantorian => H    is equivalent to Duad[H]    or JonssonTarski[H]
but we should bake these all in to one monster data structure, the Duad[H]
which is a sequence of H's with period some power of 2
these being equivalent to the "J-T ordinal" which is an H-copower of 1 in the topos of J-T algebras

one could generalize to:
instead of using Cantorian = a stream of Booleans, make it a stream of C's where C is some other Compact type
  Compact ^ Hausdorff is Compact, Hausdorff ^ Compact is Hausdorff
and by the CoCantorian Uncertainty Principle, the only Compact Hausdorff types are finitary enumerations

