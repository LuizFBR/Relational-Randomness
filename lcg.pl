% LCG step
lcg(State, A, C, M NextState) :-
    #(NextState) #= (#(A) * #(State) + #(C)) mod #(M).