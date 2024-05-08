power(0,Pow,0):-
    Pow>0.
power(Num,0,1):-
    Num>0.

power(Num,Pow,Ans):-
    Num>0,Pow>0,
    Z1 is Pow-1,
    power(Num,Z1,Result),
    Ans is Result*Num.
