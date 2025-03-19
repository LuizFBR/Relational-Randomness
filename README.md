# Relational-Randomness

Purely relational predicates that implement Pseudo-Random Number Generation (PRNG), Random Sampling (to-do) and Noise algorithms (to-do).

### Side-note: 

This project is still in its early versions and things like predicate names and argument ordering are all prone to change. This is not a (and probably will never be) "finished project".

##### To-do

- PrologDocs;
- Robust PRNG tests;
- Sampling algorithms;
- Noise generating algorithms;
- Implement other PRNGs (Mersenne Twister, LCG, etc...);

## About

This package provides the following predicates:

| Predicate          | Description |
| ------------------ | ----------- |
| rg/1               | An abbreviation of random_generator/1 |
| rg/3               | An abbreviation of random_generator/3 |
| random_generator/1 | A pre-built random generator |
| random_generator/3 | Random generator with a intial seed and sequence. |
| random/3           | Random number from 0 to 2^32-1 |
| random_bounded/4   | Random number from 0 to Max-1 |
| random_between/5   | Random number from Min to Max-1 (Max - Min < 2^32) |
| random_list/4      | List of N random numbers from 0 to 2^32-1 |
| random_boundedlist/5   | List of N random numbers from 0 to Max-1 |
| random_betweenlist/6   | List of N random numbers from Min to Max-1 (Max - Min < 2^32) |
| probability/4      | Reifies the truth value of Probability = Num / Den chance of an event happening into T |

And the following DCGs:

| Predicate                 | Description |
| ------------------------- | ----------- |
| grandom_generator//1      | Random generator with a intial seed and sequence. |
| grandom//1                | Random number from 0 to 2^32-1 |
| grandom_bounded//2        | Random number from 0 to Max-1 |
| grandom_between//3        | Random number from Min to Max-1 (Max - Min < 2^32) |
| grandom_list//2           | List of N random numbers from 0 to 2^32-1 |
| grandom_boundedlist//3    | List of N random numbers from 0 to Max-1 |
| grandom_betweenlist//4    | List of N random numbers from Min to Max-1 (Max - Min < 2^32) |
| gprobability/2            | Reifies the truth value of Probability/Precision chance of an event happening into T |

All predicates implemented so far use PCG number generation.

## How to use

### Predicates

Each predicate (except random_generator/1 ) provides the two initial arguments RandomGenerator and NextRandomGenerator, e.g.:

```prolog
random(Pcg32randomt, NextPcg32randomt, RandomInt) :-
    pcg32_random_r(Pcg32randomt, NextPcg32randomt, RandomInt).
```

And to use them you need an instance of a random generator and plug it into the first argument then into the second argument, like so:

```Prolog
?- random_generator(X),random(N1,X,X1), random_bounded(5,N2,X1,X2), random_betweenlist(-5,10,5,L1,X2,X3). 
X = pcg32randomt(9600629759793949339, 15726070495360670683),
X1 = pcg32randomt(14425053563923168794, 15726070495360670683),
N1 = 355248013,
X2 = pcg32randomt(13821270098544127085, 15726070495360670683),
N2 = 0,
X3 = pcg32randomt(2638482799351248520, 15726070495360670683),
L1 = [5, 5, -1, 3, -4].
```

### DCGs

For using the DCGs, all you need to do is call `phrase/3` (`phrase/2` won't work, as the dcgs expect to pass state) and call the predicates with their arguments without worrying about passing the number generators. _E.g._:


```Prolog
?- random_generator(X), phrase( ( grandom(A), grandom_bounded(5,B), grandom_betweenlist(-5,10,5,L) ) , [X], Y). 
X = pcg32randomt(9600629759793949339, 15726070495360670683),
A = 355248013,
B = 0,
L = [5, 5, -1, 3, -4],
Y = [pcg32randomt(2638482799351248520, 15726070495360670683)].
```

## Installation

To install ther_random library, type the following in the SWI-Prolog shell:

```Prolog
? - pack_install('r_random').
  true.
```
or, alternatively, you can install directly from this github repo:

```Prolog
?- pack_install('https://github.com/LuizFBR/r_random.git').
  true.
```

## Importing

Import this module with

```Prolog
:- use_module(library(r_random)).
```

## Dependencies

This library depends on Markus Triskas' clpfd library: https://www.swi-prolog.org/man/clpfd.html

And Ulrich Neumerkel's reif package: https://github.com/meditans/reif

## Credits

Credits for the PCG algorithms goes to:

PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation
Author: "Melissa E. O'Neill"

PCG implemenation based on https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c