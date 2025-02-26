# Relational-Randomness
Repo for storing relational predicates for give me a organized way to plan my work for implementing Pseudo-Random Number Generation (PRNG) and Random Sampling; Distribution Algorithms.

## Potential issues:

A lot of the algorithms I found seem to implement the right bit shift operator, with some very specific values, but, as we all know, right bit shifting can map a lot of different values to 0, e.g., 5 >> 3 = 0, 3 >> 3 = 0, 1 >> 3 = 0, ...
So if you start with your super specific value one way, when going backwards you could generate a not so specific value the other way ...
Which means the statistical qualities we so love and want could be completely decimated by left directional queries. 
The question is, does the reversibility keep the statistical properties that this imperative algorithm guarantees for good random values generation?
If you use it right directionally it should work just fine, though.
