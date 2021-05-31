% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(951,xfy, &).

goal(f).
goal(j).

node(a) <- node(b) & node(c).
node(b) <- node(d) & node(e).
node(c) <- node(f) & node(g).
node(d) <- node(h).
node(e) <- node(i) & node(j).
node(f) <- true.
node(j) <- true.
node(h) <- false.
node(g) <- false.
node(i) <- false.

s(a,b).
s(a,c).
s(b,d).
s(b,e).
s(c,f).
s(c,g).
s(d,h).
s(e,i).
s(e,j).

bprove(true,_D). %D = Depth
bprove((A & B),D) :-
   bprove(A,D);
   bprove(B,D).
bprove(H,D) :-
   D >= 0,
   D1 is D-1,
   (H <- B),
   nl,write(B),nl,
   bprove(B,D1).

solve_depth_limit(StartNode,Solution,ToDepth) :-
depthLimitedSearch( StartNode, Solution, ToDepth).

depthLimitedSearch( Node, [Node], _)  :-
   goal( Node).

depthLimitedSearch( Node, [Node | Sol], Maxdepth)  :-
   Maxdepth > 0,
   s( Node, Node1),
   Max1 is Maxdepth - 1,
   depthLimitedSearch( Node1, Sol, Max1).
