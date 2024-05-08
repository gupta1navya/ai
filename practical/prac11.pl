palindrome([]).
palindrome([_]).
palindrome(L):-
    append([H|T],[H],L),
    palindrome(T).
