run:-
    write("Enter X value: "),read(X),
    write("Enter Y value: "),read(Y),
    sum(X,Y).

sum(X,Y):-
    S is X+Y,
   write("Sum is: "), write(S).
