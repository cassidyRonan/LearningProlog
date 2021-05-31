:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).



prove(true).
prove((A & B)) :-
   prove(A),
   prove(B).
prove(H) :-
   (H <- B),
   prove(B).

a <- b & c.
b <- true.
c <- false.
