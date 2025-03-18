

:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).


:- op(700, xfx, :=:).  % Rational number equality

% Reification operators:
:- op(760, yfx, :<==>:). % P :<==>: Q	True iff P and Q are equivalent
:- op(750, xfy, :==>:).  % P :==>: Q	True iff P implies Q
:- op(750, yfx, :<==:).  % P :<==: Q	True iff Q implies P
:- op(740, yfx, :\/:).   % P :\/: Q	    True iff either P or Q
:- op(730, yfx, :\:).    % P :\: Q	    True iff either P or Q, but not both
:- op(720, yfx, :/\:).   % P :/\: Q	    True iff both P and Q
:- op(710,  fy, :\).     % :\ Q	        True iff Q is false

% Comparison operators:
:- op(700, xfx, :>:).    % Expr1 #> Expr2	 Expr1 is greater than Expr2
:- op(700, xfx, :<:).    % Expr1 #< Expr2	 Expr1 is less than Expr2
:- op(700, xfx, :>=:).   % Expr1 #>= Expr2	 Expr1 is greater than or equal to Expr2
:- op(700, xfx, :=<:).   % Expr1 #=< Expr2	 Expr1 is less than or equal to Expr2
:- op(700, xfx, :=:).    % Expr1 #= Expr2	 Expr1 equals Expr2
:- op(700, xfx, :\=:).   % Expr1 #\= Expr2	 Expr1 is not equal to Expr2

% Domain operators:
% :- op(700, xfx, in).
% :- op(700, xfx, ins).
% :- op(450, xfx, .:.). % should bind more tightly than \/ - equivalent clpfd to ..
% :- op(150, fx, :). % equivalent to clpfd #

% R1 is less than R2.
P1 / Q1 :<: P2 / Q2 :-
    #(P1) * #(Q2) #< #(P2) * #(Q1).

% clpq_expression(Exp1 / Exp2) -> clpq_expression / clpq_expression. % base case
% clpq_expression(Exp1 + Exp2) -> clpq_expression + clpq_expression
% clpq_expression(Exp1 - Exp2) -> ...
% clpq_expression(Exp1 * Exp2) ->
% clpq_expression(Exp1 ^ Exp2) ->

clpq_expression(Expression) --> term(T), expression_rest(T, Expression).

expression_rest(Acc, Expression) -->
    ("+" , term(T), { NewAcc = Acc + T }, expression_rest(NewAcc, Expression))
    ; ("-" , term(T), { NewAcc = Acc - T }, expression_rest(NewAcc, Expression))
    ; [].

clpq_expression_rest(Acc, Expr) -->
    "+", term(Term), { Acc1 = Acc + Term }, clpq_expression_rest(Acc1, Expr).
clpq_expression_rest(Acc, Expr) -->
    "-", term(Term), { Acc1 = Acc - Term }, clpq_expression_rest(Acc1, Expr).
clpq_expression_rest(Expr, Expr) --> [].

term(Expr) --> factor(Expr), term_rest(Expr).

term_rest(Acc, Term) -->
    ("*" , factor(F), { NewAcc = Acc * F }, term_rest(NewAcc, Term))
    ; ("/" , factor(F), { NewAcc = Acc / F }, term_rest(NewAcc, Term))
    ; [].



factor(Expr) --> number(Expr).
factor(Expr) --> "(", clpq_expression(Expr), ")".