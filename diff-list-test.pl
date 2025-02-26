:- use_module(library(clpfd)).
:- set_prolog_flag(clpfd_monotonic, true).

inc_1(List,IncList) :-
    inc_1_diff_list(List,IncList-[]).

inc_1_diff_list([],Acc-Acc).
inc_1_diff_list([X | T], [X1|Rest]-Tail) :-
    prolog_current_frame(Frame),
    format("~w~n", [Frame]),
    #(X1) #= #(X) + 1,
    inc_1_diff_list(T,Rest-Tail).

inc_1_notco([],[]).
inc_1_notco([X|T],[X1|T1]) :-
    prolog_current_frame(Frame),
    format("~w~n", [Frame]),
    #(X1) #= #(X) + 1,
    inc_1_notco(T,T1).


inc_1_ndf(List,IncList) :-
    inc_1_(List,[],RevIncList),
    reverse(IncList,RevIncList).

inc_1_([],Acc,Acc).
inc_1_([X|T],Acc,IncList) :-
    prolog_current_frame(Frame),
    format("~w~n", [Frame]),
    #(X1) #= #(X) + 1,
    inc_1_(T,[X1|Acc],IncList).


tailcall([]).
tailcall([_|Xs]) :-
    prolog_current_frame(Frame),
    format("~w~n", [Frame]),
    tailcall(Xs).

not_tailcall([]).
not_tailcall([_|Xs]) :-
    not_tailcall(Xs),
    prolog_current_frame(Frame),
    format("~w~n", [Frame]).