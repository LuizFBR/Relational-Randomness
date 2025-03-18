:- module(r_random,
[
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
random_generator(pcg32randomt(0x853c49e6748fea9b, 0xda3e39cb94b95bdb)).

random_generator(InitState, InitSeq, Pcg32randomt) :-
    pcg32_srandom_r(InitState, InitSeq, Pcg32randomt).

% Random integer in [0,2^32-1].
random(Pcg32randomt, NextPcg32randomt, RandomInt) :-
    pcg32_random_r(Pcg32randomt, NextPcg32randomt, RandomInt).

grandom(RandomInt) --> state( Pcg32randomt, NextPcg32randomt ),
    { pcg32_random_r(Pcg32randomt, NextPcg32randomt, RandomInt) }.

% Random integer in [0,Max-1].
random_bounded(Pcg32randomt, NextPcg32randomt, Max, RandomInt) :-
    pcg32_boundedrand_r(Pcg32randomt, NextPcg32randomt, Max, RandomInt).

grandom_bounded(Max,RandomInt) --> state( Pcg32randomt, NextPcg32randomt ),
    { pcg32_boundedrand_r(Pcg32randomt, NextPcg32randomt, Max, RandomInt) }.

% Random integer in [Min,Max].
random_between(Pcg32randomt, NextPcg32randomt, Min, Max, RandomInt) :-
    #(Max) - #(Min) #< 2^32, #(Min) #< #(Max),
    #(Bound) #= #(Max) - #(Min),
    pcg32_boundedrand_r(Pcg32randomt, NextPcg32randomt,Bound,R),
    #(RandomInt) #= #(R) + #(Min).

grandom_between(Min, Max, RandomInt) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_between(Pcg32randomt, NextPcg32randomt, Min, Max, RandomInt) }.

% Relationship between a natural number N and a list of random numbers L such that length(L,N).
random_list(Pcg32randomt, FinalPcg32randomt, N, RL) :-
    #(N) #> 0 #<==> #(T),
    if_(   T = 0
       , ( Pcg32randomt = FinalPcg32randomt, RL = [] )
       , ( #(N1) #= #(N) - 1,
           pcg32_random_r(Pcg32randomt, NextPcg32randomt, RandomInt),
           RL = [ RandomInt | RandomInts ],
           random_list(NextPcg32randomt, FinalPcg32randomt, N1, RandomInts)
         )
       ).

grandom_list(N, RL) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_list(Pcg32randomt, NextPcg32randomt, N, RL) }.

% Relationship between a natural number N and a list of random bounded numbers L such that length(L,N).
random_boundedlist(Pcg32randomt, FinalPcg32randomt, Bound, N, RL) :-
    #(N) #> 0 #<==> #(T),
    if_(   T = 0
       , ( Pcg32randomt = FinalPcg32randomt, RL = [] )
       , ( #(N1) #= #(N) - 1,
           pcg32_boundedrand_r(Pcg32randomt, NextPcg32randomt, Bound, RandomInt),
           RL = [ RandomInt | RandomInts ],
           random_boundedlist(NextPcg32randomt, FinalPcg32randomt, Bound, N1, RandomInts)
         )
       ).

grandom_boundedlist(Bound, N, RL) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_boundedlist(Pcg32randomt, NextPcg32randomt, Bound, N, RL) }.

% Relationship between a natural number N and a list of random numbers between Min and Max) L such that length(L,N).
random_betweenlist(Pcg32randomt, FinalPcg32randomt, Min, Max, N, RL) :-
    #(N) #> 0 #<==> #(T),
    if_(   T = 0
       , ( Pcg32randomt = FinalPcg32randomt, RL = [] )
       , ( #(N1) #= #(N) - 1,
           random_between(Pcg32randomt, NextPcg32randomt, Min, Max, RandomInt),
           RL = [ RandomInt | RandomInts ],
           random_betweenlist(NextPcg32randomt, FinalPcg32randomt, Min, Max, N1, RandomInts)
         )
       ).
    
grandom_betweenlist(Min, Max, N, RL) --> state( Pcg32randomt, NextPcg32randomt ),
    { random_betweenlist(Pcg32randomt, NextPcg32randomt, Min, Max, N, RL) }.

% T is reified - if an event with probability Probability/Precision happens, T = 1, else T = 0.
probability(Pcg32randomt, NextPcg32randomt, Probability/Precision, T) :-
    pcg32_boundedrand_r(Pcg32randomt, NextPcg32randomt, Precision, RandomNumber),
    #(RandomNumber) #=< #(Probability) #<==> #(T).

gprobability(Probability/Precision, T) --> state( Pcg32randomt, NextPcg32randomt ),
    { probability( Pcg32randomt, NextPcg32randomt, Probability/Precision, T ) }.

% -------------------- private predicates ---------------------------:

% Auxiliary dcgs:

state(S0, S), [S] --> [S0]. % implicitly pass states.

% Random Number Generator
pcg32_srandom_r( InitState, InitSeq, FinalPcg32randomt) :-
    Sup is 2^64 - 1, InitState in 0..Sup, InitSeq in 0..Sup,
    1 #= #(InitSeq) /\ 1,
    #(Inc) #= ( ( #(InitSeq) << 1) \/ 1 ) mod Sup,
    pcg32_random_r(pcg32randomt(0,Inc), pcg32randomt(S2,Inc), _),
    #(S3) #= ( #(S2) + #(InitState) ) mod Sup,
    pcg32_random_r( pcg32randomt(S3,Inc), FinalPcg32randomt, _).

% Uniformly distributed 32-bit random number:
pcg32_random_r( pcg32randomt(OldState,Inc), pcg32randomt(NextState,Inc), RandomValue ) :-
    #(NextState) #= ( ( #(OldState) * 6364136223846793005) + (#(Inc) \/ 1) ) mod (2^64),
    #(XorShifted) #=( ( ( #(OldState) >> 18) xor #(OldState)) >> 27) mod (2^32), % here and in the line below mod 2^32 might be unnecessary, but i'm using it for precaution.
    #(Rot) #= ( #(OldState) >> 59) mod (2^32),
    #(RandomValue) #= ( (#(XorShifted) >> #(Rot)) \/ (#(XorShifted) << (- #(Rot) /\ 31)) ) mod (2^32).

% Bounded random number using rejection sampling
pcg32_boundedrand_r( Pcg32randomt, NextPcg32randomt, Bound, Result) :-
    Sup is 2^32, Bound in 1..Sup,
    bounded_sample( Pcg32randomt, NextPcg32randomt, Bound, Result).

% Rejection sampling:
bounded_sample(Pcg32randomt, FinalPcg32randomt, Bound, Result) :-
    pcg32_random_r(Pcg32randomt, NextPcg32randomt, R),
    #(R) #>= 2^32 mod #(Bound) #<==> #(T), % had to remove -bound mod bound optimization, since it only worked for 32 bits representations.
    if_(   T = 1
       , ( #(Result) #= #(R) mod #(Bound), FinalPcg32randomt = NextPcg32randomt )
       ,   bounded_sample(NextPcg32randomt, FinalPcg32randomt, Bound, Result)
    ).