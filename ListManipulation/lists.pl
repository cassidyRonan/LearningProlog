% Computational Intelligence: a logical approach.
% Prolog Code. Section 3.4.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% append(X,Y,Z) is true when X, Y and Z are lists
% and Z contains the elements of X (in order)
% followed by the elements of Y (in order)
mappend([],Z,Z).
mappend([A|X],Y,[A|Z]) :-
   mappend(X,Y,Z).

% member(X,L) is true if X is an element of list L
mmember(X,[X|_]).
mmember(X,[_|R]):-
   mmember(X,R).

% rev(L,R) that is true if list R contains the same
% elements as list L, but in reverse order.
mrev([],[]).
mrev([H|T],R):-
   mrev(T,RT),
   append(RT,[H],R).

% reverse(L,R) is true if R contains the lements of
% L, but in reverse order.
mreverse(L,R):-
   rev3(L,[],R).

% rev3(L,A,R) is true if R contains the elements of
% L in reverse order followed by the elements of A.
rev3([],L,L).
rev3([H|T],A,R):-
   rev3(T,[H|A],R).


writeList([]) :- nl.
writeList([H|T]) :-
    write(H), nl,
    writeList(T).

seperate_list([],[],[]).

seperate_list([X,L],[X|L1],L2):-
   atom(X),
   seperate_list(L,L1,L2).

seperate_list([X|L],L1,[X|L2]):-
   seperate_list(L,L1,L2).

/* 
separate_list([],[],[]).
separate_list([X|Xs],Y,Z):-
   ( atom(X) -> (Y,Z)=([X|Ys],Zs); (integer(X) -> (Y,Z)=(Ys,[X|Zs])) ), separate_list(Xs,Ys,Zs). */
   
separate_list([],[],[]).
separate_list([X|Xs],Y,Z):-
   (atom(X) -> (Y,Z) = ([X|Ys],Zs); %if true
   integer(X) -> (Y,Z)=(Ys,[X|Zs])), %else then
   separate_list(Xs,Ys,Zs). 