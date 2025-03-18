:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic,true).


A :=: B --> clpq_expression(A), clpq_expression(B).

% Expression -> Term + Expression | Term - Expression | Term
clpq_expression(Value) -->
    term(T1), clpq_expression_rest(T1, Value).

clpq_expression_rest(T1, Value) -->
    "+", term(T2), { Value #= T1 + T2 }, clpq_expression_rest(Value, Value).
clpq_expression_rest(T1, Value) -->
    "-", term(T2), { Value #= T1 - T2 }, clpq_expression_rest(Value, Value).
clpq_expression_rest(Value, Value) --> [].

% Term -> Factor * Term | Factor // Term | Factor
term(Value) -->
    factor(F1), rest_term(F1, Value).

rest_term(F1, Value) --> "*", factor(F2), { #(Value) #= #(F1) * #(F2) }, rest_term(Value, Value).
rest_term(F1, Value) --> "/", factor(F2), { F2 #\= 0, #(Value) #= F1 / F2 }, rest_term(Value, Value).
rest_term(Value, Value) --> [].

% Factor -> Number | ( Expression )
factor(Value) -->
    number(Value).
factor(Value) -->
    "(", expression(Value), ")".

% Number -> Sequence of digits interpreted as an integer
number(Value) -->
    digit(D), digits(Ds),
    { foldl(build_number, Ds, D, Value) }.

% Single digit as an integer
digit(D) --> [C], { C in 48..57, D #= C - 48 }.

% Sequence of digits
digits([D|Ds]) --> digit(D), digits(Ds).
digits([]) --> [].

% Convert list of digits into an integer
build_number(D, Acc, Value) :-
    Value #= Acc * 10 + D.