
max(X,Y,M):-
 X>=Y,
 M is X,write("Max: "),write(M).
max(X,Y,M):-
 Y>=X,
 M is Y,write("Max: "),write(M).
