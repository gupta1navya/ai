conc([],L2,L2).
conc([H|T1],L1,[H|T2]):-
    conc(T1,L1,T2).
