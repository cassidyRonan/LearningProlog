:- op(1150, xfx, <- ).
:- op(950,xfy, &).

:- dynamic answered/2.

prove(G) :-
   wprove(G,[]).
show(G):-
   wprove(G,T),
   traverse(T).


% wprove(G,Anc) is true if G can be proven with ancestor list Anc.
% This also asks the user and lets the user ask "why".
wprove(true,_).
wprove((A & B), (AT&BT)):-
   wprove(A,AT),
   wprove(B,BT).
wprove((A & B), (Anc)):-
   wprove(A,Anc),
   wprove(B,Anc).
wprove(H,_):-
   builtin(H),
   call(H).
/* wprove(H,if(H,BT)):-
   (H <- B),
   wprove(B,BT). */
wprove(H,Anc) :-
   (H <- B),
   wprove(B,[(H <- B)|Anc]).
wprove(G,_) :-
   askable(G),
   isAnswered(G,Val),
   (Val = yes ; integer(Val)).
wprove(G,Anc) :-
   askable(G),
   unanswered(G),
   ask(G,Ans,Anc),
   assert(answered(G,Ans)),
   (Ans=yes ; integer(Ans)).



% answered(G,Ans) is dynamically added to the database.
:- dynamic answered/2.

% unanswered(G) is true if G has not be answered
unanswered(G) :- \+ answered(G,_).   % negation as failure
isAnswered(G,Value) :- answered(G,Value).

% ask(G,Ans) asks the user G, and the user relies with Ans
ask(G,Ans,Anc) :-
   writeln(['Is ',G,' true? (yes/no) ']),
   read(Reply),
   interpret(G,Ans,Reply,Anc).

% interpret(G,Ans,Reply,Anc) means that we should interpret Reply to question
%   G as answer Ans in the context of ancesort list Anc
interpret(_,Reply,Reply,_) :-
   Reply \== why.
interpret(G,Ans,why,[Rule|T]):-
   writeln(['I used the rule: ',Rule,'.']),
   ask(G,Ans,T).
interpret(G,Ans,why,[]):-
   writeln(['Because that is what you asked me!']),
   ask(G,Ans,[]).


% writeln(L) writes each element of list L
writeln([]) :- nl.
writeln([H|T]) :-
   write(H),
   writeln(T).

% To implement Example 6.12
% | ?- aprove(lit(L)).
% traverse(T) true if T is a tree being traversed
traverse(if(H,true)) :-
   writeln([H,' is a fact']).
traverse(if(H,builtin)) :-
   writeln([H,' is built-in.']).
traverse(if(H,B)) :-
   B \== true,
   B \== builtin,
   writeln([H,' :-']),
   printbody(B,1,_),
   read(Comm),
   interpretcommand(Comm,B).

% printbody(B,N1,N2) is true if B is a body to be printed and N1 is the
% count of atoms before B was called and N2 is the count after
printbody(true,N,N).
printbody((A&B),N1,N3) :-
  printbody(A,N1,N2),
  printbody(B,N2,N3).
printbody(if(H,_),N,N1) :-
  writeln(['   ',N,': ',H]),
  N1 is N+1.

% interpretcommand(Comm,B) interprets the command Comm on body B
interpretcommand(N,B) :-
  integer(N),
  nth(B,N,E),
  traverse(E).

% nth(S,N,E) is true if E is the N-th element of structure S
nth(A,1,A) :-
  \+ (A = (_,_)).
nth((A&_),1,A).
nth((_&B),N,E) :-
  N>1,
  N1 is N-1,
  nth(B,N1,E).

builtin((_ =< _)).
builtin((_ >= _)).
builtin((_ = _)).
builtin((_ < _)).
builtin((_ > _)).
