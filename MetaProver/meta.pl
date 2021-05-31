:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

a <- b.
a <- c.
b <- d.
b <- e.
c <- f.
c <- g.
d <- h.
e <- i.
e <- j.
f <- true.
j <- true.



bprove(true,_D).

bprove((A&B),D):-
    bprove(A,D),
    bprove(B,D).

bprove(H,D):-
    D>=0,
    D1 is D-1,
    (H<-B),
    bprove(B,D1).
