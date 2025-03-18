# Relational-Randomness

Purely relational predicates that implement Pseudo-Random Number Generation (PRNG) and Random Sampling (to-do).

##### To-do

- Robust PRNG tests;
- Sampling algorithms;
- Noise generating algorithms;
- Implement other PRNGs (Mersenne Twister, LCG, etc...);
- DCG argument passing.

## About

This repo provides the following predicates:

| Predicate          | Description |
| ------------------ | ----------- |
| random_generator/1 | A pre-built random generator |
| random_generator/3 | Random generator with a intial seed and sequence. |
| random/3           | Random number from 0 to 2^32-1 |
| random_bounded/4   | Random number from 0 to Max-1 |
| random_between/5   | Random number from Min to Max-1 (Max - Min < 2^32) |
| random_list/4      | List of N random numbers from 0 to 2^32-1 |
| random_bounded/4   | List of N random numbers from 0 to Max-1 |
| random_between/5   | List of N random numbers from Min to Max-1 (Max - Min < 2^32) |
| probability/4      | Reifies the truth value of Probability/Precision chance of an event happening into T |

## How to use

## PCG

Each predicate (except random_generator/1 ) provides the two initial arguments RandomGenerator and NextRandomGenerator, e.g.:

```prolog
random(Pcg32randomt, NextPcg32randomt, RandomInt) :-
    pcg32_random_r(Pcg32randomt, NextPcg32randomt, RandomInt).
```

And to use them you need an instance of a random generator and plug it into the first argument then into the second argument, like so:

```Prolog
?- random_generator(X),random(X,X1,N1), random_bounded(X1,X2,5,N2), random_betweenlist(X2,X3,-5,10,5,L1). 
X = pcg32randomt(9600629759793949339, 15726070495360670683),
X1 = pcg32randomt(14425053563923168794, 15726070495360670683),
N1 = 355248013,
X2 = pcg32randomt(13821270098544127085, 15726070495360670683),
N2 = 0,
X3 = pcg32randomt(2638482799351248520, 15726070495360670683),
L1 = [5, 5, -1, 3, -4].
```

## Credits

Credits for the PCG algorithms goes to:

PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation
Author: "Melissa E. O'Neill"

PCG implemenation based on https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c