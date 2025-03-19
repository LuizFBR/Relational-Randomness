:- module(r_random,
[
    rg/1,
    rg/3,
    random_generator/1,
    random_generator/3,
    random/3,
    grandom//1,
    random_bounded/4,
    grandom_bounded//2,
    random_between/5,
    grandom_between//3,
    random_list/4,
    grandom_list//2,
    random_boundedlist/5,
    grandom_boundedlist//3,
    random_betweenlist/6,
    grandom_betweenlist//4,
    probability/4,
    gprobability//2
]).

:- use_module(library(reif)).
:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).

% reference Minimal C Implementation: https://www.pcg-random.org/download.html
% PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation
% Author: "Melissa E. O'Neill"
% based on https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c

% typedef struct { uint64_t state;  uint64_t inc; } pcg32_random_t;
% pcg32randomt(State,Inc).

% -------------------- public predicates ---------------------------:

% A default random number generator.
random_generator( pcg32randomt(0x853c49e6748fea9b, 0xda3e39cb94b95bdb) ).

random_generator( InitState, InitSeq, Pcg32randomt ) :-
    pcg32_srandom_r( InitState, InitSeq, Pcg32randomt ).

% abbreviated functor name for random number generator.
rg( pcg32randomt( 0x853c49e6748fea9b, 0xda3e39cb94b95bdb ) ).
rg( InitState, InitSeq, Pcg32randomt ) :-
    pcg32_srandom_r( InitState, InitSeq, Pcg32randomt ).

% Random integer in [0,2^32-1].
random( RandomInt, Pcg32randomt, NextPcg32randomt ) :-
    pcg32_random_r( RandomInt, Pcg32randomt, NextPcg32randomt ).

grandom(RandomInt) --> state( Pcg32randomt, NextPcg32randomt ),
    { pcg32_random_r( RandomInt, Pcg32randomt, NextPcg32randomt ) }.

% Random integer in [0,Max-1].
random_bounded( Max, RandomInt, Pcg32randomt, NextPcg32randomt ) :-
    pcg32_boundedrand_r( Max, RandomInt, Pcg32randomt, NextPcg32randomt ).

grandom_bounded( Max, RandomInt ) --> state( Pcg32randomt, NextPcg32randomt ),
    { pcg32_boundedrand_r( Max, RandomInt, Pcg32randomt, NextPcg32randomt ) }.

% Random integer in [Min,Max].
random_between( Min, Max, RandomInt, Pcg32randomt, NextPcg32randomt ) :-
    #(Max) - #(Min) #< 2^32, #(Min) #< #(Max),
    #(Bound) #= #(Max) - #(Min),
    pcg32_boundedrand_r( Bound, R, Pcg32randomt, NextPcg32randomt ),
    #(RandomInt) #= #(R) + #(Min).

grandom_between(Min, Max, RandomInt) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_between( Min, Max, RandomInt, Pcg32randomt, NextPcg32randomt ) }.

% Relationship between a natural number N and a list of random numbers L such that length(L,N).

random_list( N, RL, Pcg32randomt, FinalPcg32randomt ) :-
    length(RL,N), foldl( random, RL, Pcg32randomt, FinalPcg32randomt ).

grandom_list(N, RL) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_list( N, RL, Pcg32randomt, NextPcg32randomt) }.

% Relationship between a natural number N and a list of random bounded numbers L such that length(L,N).
random_boundedlist( Bound, N, RL, Pcg32randomt, FinalPcg32randomt ) :-
    length(RL,N), foldl( random_bounded(Bound), RL, Pcg32randomt, FinalPcg32randomt ).

grandom_boundedlist( Bound, N, RL ) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_boundedlist( Bound, N, RL, Pcg32randomt, NextPcg32randomt ) }.

% Relationship between a natural number N and a list of random numbers between Min and Max) L such that length(L,N).
random_betweenlist( Min, Max, N, RL, Pcg32randomt, FinalPcg32randomt ) :-
    length(RL,N), foldl( random_between(Min,Max), RL, Pcg32randomt, FinalPcg32randomt ).
    
grandom_betweenlist( Min, Max, N, RL ) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_betweenlist( Min, Max, N, RL, Pcg32randomt, NextPcg32randomt ) }.

% T is reified - if an event with probability Probability/Precision happens, T = 1, else T = 0.
probability( Probability/Precision, T, Pcg32randomt, NextPcg32randomt ) :-
    pcg32_boundedrand_r( Precision, RandomNumber, Pcg32randomt, NextPcg32randomt ),
    #(RandomNumber) #=< #(Probability) #<==> #(T).

gprobability(Probability/Precision, T) --> state( Pcg32randomt, NextPcg32randomt ),
    { probability( Probability/Precision, T, Pcg32randomt, NextPcg32randomt ) }.

% -------------------- private predicates ---------------------------:

% Auxiliary dcgs:

state(S0, S), [S] --> [S0]. % implicitly pass states.

% Random Number Generator
pcg32_srandom_r( InitState, InitSeq, FinalPcg32randomt ) :-
    Sup is 2^64 - 1, InitState in 0..Sup, InitSeq in 0..Sup,
    1 #= #(InitSeq) /\ 1,
    #(Inc) #= ( ( #(InitSeq) << 1) \/ 1 ) mod Sup,
    pcg32_random_r( _, pcg32randomt(0,Inc), pcg32randomt(S2,Inc) ),
    #(S3) #= ( #(S2) + #(InitState) ) mod Sup,
    pcg32_random_r( _, pcg32randomt(S3,Inc), FinalPcg32randomt ).

% Uniformly distributed 32-bit random number:
pcg32_random_r( RandomValue, pcg32randomt(OldState,Inc), pcg32randomt(NextState,Inc) ) :-
    #(NextState) #= ( ( #(OldState) * 6364136223846793005) + (#(Inc) \/ 1) ) mod (2^64),
    #(XorShifted) #=( ( ( #(OldState) >> 18) xor #(OldState)) >> 27) mod (2^32), % here and in the line below mod 2^32 might be unnecessary, but i'm using it for precaution.
    #(Rot) #= ( #(OldState) >> 59) mod (2^32),
    #(RandomValue) #= ( (#(XorShifted) >> #(Rot)) \/ (#(XorShifted) << (- #(Rot) /\ 31)) ) mod (2^32).

% Bounded random number using rejection sampling
pcg32_boundedrand_r( Bound, Result, Pcg32randomt, NextPcg32randomt ) :-
    Sup is 2^32, Bound in 1..Sup,
    #(Threshold) #= 2^32 mod #(Bound),
    bounded_sample( Bound, Threshold, Result, Pcg32randomt, NextPcg32randomt ).

% Rejection sampling:
bounded_sample( Bound, Threshold, Result, Pcg32randomt, FinalPcg32randomt ) :-
    pcg32_random_r(R, Pcg32randomt, NextPcg32randomt),
    (   #(R) #>= #(Threshold)
    ->  #(Result) #= #(R) mod #(Bound),
        FinalPcg32randomt = NextPcg32randomt
    ;   bounded_sample( Bound, Threshold, Result, NextPcg32randomt, FinalPcg32randomt )
    ).