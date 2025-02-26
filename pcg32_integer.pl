:- module(pcg32_integer,
    [pcg32_random_r/3,
    pcg32randomt_initializer/1]).

:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).


% reference Minimal C Implementation: https://www.pcg-random.org/download.html
% PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation
% Author: "Melissa E. O'Neill"
% based on https://github.com/imneme/pcg-c-basic/blob/master/pcg_basic.c


% -------------------- public predicates ---------------------------:

% A default random number generator.
pcg32randomt_initializer(pcg32randomt(0x853c49e6748fea9b, 0xda3e39cb94b95bdb)).

random_bounded(Pcg32randomt,NewPcg32randomt,Max,RandomInt) :-
    pcg32_boundedrand_r(Pcg32randomt,NewPcg32randomt,Max,RandomInt).

random_between(Pcg32randomt,NewPcg32randomt,Min,Max,RandomInt) :-
    pcg32_boundedrand_r(Pcg32randomt,NewPcg32randomt,Max,R),
    #(RandomInt) #= #(R) + #(Min).

% Relationship between a natural number N and a list of random numbers L such that length(L,N).
random_list(Pcg32randomt, Pcg32randomt, 0, []).
random_list(Pcg32randomt, FinalPcg32randomt, N, [RandomInt|RandomInts]) :-
    #(N) #> 0,
    #(N1) #= #(N) - 1,
    pcg32_random_r(Pcg32randomt, NextPcg32randomt, RandomInt),
    random_list(NextPcg32randomt, FinalPcg32randomt, N1, RandomInts).

random_boundedlist(Pcg32randomt, Pcg32randomt, _, 0, []).
random_boundedlist(Pcg32randomt, FinalPcg32randomt, Bound, N, [RandomInt|RandomInts]) :-
    #(N) #> 0,
    #(N1) #= #(N) - 1,
    pcg32_boundedrand_r(Pcg32randomt, NextPcg32randomt, Bound, RandomInt),
    random_boundedlist(NextPcg32randomt, FinalPcg32randomt, Bound, N1, RandomInts).

random_betweenlist(Pcg32randomt, Pcg32randomt, _, 0, []).
random_betweenlist(Pcg32randomt, FinalPcg32randomt, MinInt, MaxInt, N, [RandomInt|RandomInts]) :-
    #(N) #> 0,
    #(N1) #= #(N) - 1,
    random_between(Pcg32randomt, NextPcg32randomt, MinInt, MaxInt, RandomInt),
    random_betweenlist(NextPcg32randomt, FinalPcg32randomt, MinInt, MaxInt, N1, RandomInts).

random_float()


% -------------------- private predicates ---------------------------:

% Use this to initialize a generator.
% pcg32randomt_initstate_initseq_newpcg32randomt(Pcg32randomt,InitState,InitSeq,NewPcg32randomt) :-
pcg32_srandom_r(InitState,InitSeq,NewPcg32randomt) :-
    pcg32randomt_state(Pcg32randomt_1,0),
    #(Inc) #= (#(InitSeq) << 1) \/ 1,
    pcg32randomt_inc(Pcg32randomt_1, Inc),
    pcg32_random_r(Pcg32randomt_1,Pcg32randomt_2,_),
    pcg32randomt_state(Pcg32randomt_2,S2),
    #(S3) #= #(S2) + InitState,
    pcg32randomt_state(Pcg32randomt_3,S3),
    pcg32randomt_inc(Pcg32randomt_3,Inc),
    pcg32_random_r(Pcg32randomt_3,NewPcg32randomt,_).

% typedef struct { uint64_t state;  uint64_t inc; } pcg32_random_t;
pcg32randomt_state(pcg32randomt(State,_),State) :- % RNG state.  All values are possible.
    #(State) #>= 0,
    #(State) #< 2^64.
pcg32randomt_inc(pcg32randomt(_,Inc),Inc) :- % Controls which RNG sequence (stream) is selected. Must *always* be odd.
    #(Inc) #>= 0,
    #(Inc) #< 2^64,
    1 #= #(Inc) /\ 1. % Inc is odd
% Generate a uniformly distributed 32-bit random number:
% pcg32_random_r(+Pcg32randomt, -NextState, -RandomValue)
pcg32_random_r(Pcg32randomt, NewPcg32randomt, RandomValue) :-
    pcg32randomt_state(Pcg32randomt,State),
    #(State) #>= 0,
    pcg32randomt_inc(Pcg32randomt,Inc),
    #(Inc) #>= 0,
    #(NextState) #= ((#(State) * 6364136223846793005) + (#(Inc) \/ 1) ) mod (2^64),
    pcg32randomt_state(NewPcg32randomt, NextState),
    pcg32randomt_inc(Pcg32randomt,Inc),
    #(XorShifted) #=( ((#(State) >> 18) xor #(State)) >> 27) mod (2^32),
    #(Rot) #= (#(State) >> 59) mod (2^32),
    #(RandomValue) #= ((#(XorShifted) >> #(Rot)) \/ (#(XorShifted) << (- #(Rot) /\ 31))) mod (2^32).

% Generate a bounded random number using rejection sampling
pcg32_boundedrand_r(Pcg32randomt,NewPcg32randomt,Bound,Result) :-
    #(Bound) #> 0,  % Ensure bound is positive
    bounded_sample(Pcg32randomt,NewPcg32randomt,Bound,Result).

% Rejection sampling:
bounded_sample(Pcg32randomt,NewPcg32randomt,Bound,Result) :-
    pcg32_random_r(Pcg32randomt,NewPcg32randomt, R),
    #(R) #>= - #(Bound) mod #(Bound),
    #(Result) #= #(R) mod #(Bound).

bounded_sample(Pcg32randomt,NewPcg32randomt,Bound,Result) :-
    pcg32_random_r(Pcg32randomt,Pcg32randomt_2,R),  % Discard invalid value
    #(R) #< - #(Bound) mod #(Bound),
    bounded_sample(Pcg32randomt_2,NewPcg32randomt,Bound,Result).