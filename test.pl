:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).

cg(State, NextState) :-
    lcg_params(a, A),
    lcg_params(c, C),
    #(NextState) #= (A * #(State) + C) mod 64. % prolog works with 64 bit integers.

% Permute the output using a simple bitwise XOR (or another permutation function)
permute(Value, PermutedValue) :-
    PermutedValue #= Value xor (Value >> 16).