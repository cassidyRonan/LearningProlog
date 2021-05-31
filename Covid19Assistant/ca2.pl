:- op(1150, xfx, <- ).
:- op(950,xfy, &).
:- ensure_loaded(covidRules).
:- ensure_loaded(combinedProver).
:- ensure_loaded(languageParser).
:- dynamic level/1.

start:-
   print('Welcome to the national framework for living with COVID-19 assistant!'),nl,
   print('How can I help you today?'),nl,!,
   readIn(T),
   query(T).

query:-
   !,nl,write('Is there anything else I can assist you with?'),nl,
   readIn(T),
   query(T).

readIn(T):-
    write('>'), read_str(S), tokenize(S,T), process(T).

query([stop]):-
   nl.
query(help):-
   nl,
   write('You can ask me the following questions:'),nl,
   write('What are the guidelines for level [number]?'),nl.
query(_):-
   query.

getRestrictions(X):-
   bagof(H,(H <- level(X)),List),
   writeRestrictionList(List).

writeRestrictionList([]):- nl.
writeRestrictionList([restriction(X,Y)|T]):-
   %(H <- restriction(X,Y)),
   write(X),write(' - '),write(Y),nl,
   writeRestrictionList(T).


% read_str(-String)
%   Accepts a whole line of input as a string (list of ASCII codes).
%   Assumes that the keyboard is buffered.

read_str(String) :- get0(Char),
                    read_str_aux(Char,String).

read_str_aux(-1,[]) :- !.    % end of file
read_str_aux(10,[]) :- !.    % end of line (UNIX)
read_str_aux(13,[]) :- !.    % end of line (DOS)

read_str_aux(Char,[Char|Rest]) :- read_str(Rest).

