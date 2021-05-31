:- ensure_loaded(covidRules).
% read_str(-String)
%   Accepts a whole line of input as a string (list of ASCII codes).
%   Assumes that the keyboard is buffered.

read_str(String) :- get0(Char),
                    read_str_aux(Char,String).

read_str_aux(-1,[]) :- !.    % end of file
read_str_aux(10,[]) :- !.    % end of line (UNIX)
read_str_aux(13,[]) :- !.    % end of line (DOS)

read_str_aux(Char,[Char|Rest]) :- read_str(Rest).


% read_atom(-Atom)
%  Reads a line of input and converts it to an atom.
%  See text concerning name/2 vs. atom_codes/2.

read_atom(Atom) :-
   read_str(String),
   name(Atom,String).    % or preferably atom_codes(Atom,String).


% read_num(-Number)
%  Reads a line of input and converts it to a number.
%  See text concerning name/2 vs. number_codes/2.

read_num(Atom) :-
   read_str(String),
   name(Atom,String).    % or preferably number_codes(Atom,String).


% write_str(+String)
%  Outputs the characters corresponding to a list of ASCII codes.

write_str([Code|Rest]) :- put(Code), write_str(Rest).
write_str([]).


%  TOKENISER
get_tokens(String,TokenList) :-
  string_codes(String,CharList), % convert to a list of ASCII codes
  tokenize(CharList,TokenList).


tokenize([],[]) :- !.

tokenize(String,[Word|Rest]) :-
  grab_word(String,Chars,NewString),
  name(Word,Chars),
  tokenize(NewString,Rest).


% grab_word(+String,-Chars,-Rest)
%  Splits String into the characters constituting
%  the first token (Chars) and the remainder (Rest).

grab_word([32|Tail],[],Tail) :- !.      % stop upon hitting a blank


grab_word([],[],[]).                    % stop at end of list
grab_word([Char|Tail],Chars,Rest) :-    % skip punctuation marks
  punctuation_mark(Char), !,
  grab_word(Tail,Chars,Rest).

grab_word([Char|Tail1],[NewChar|Tail2],Rest) :-
  grab_word(Tail1,Tail2,Rest),
  lower_case(Char,NewChar).             % if within a word, keep going

% punctuation_mark(+Char)
%  Succeeds if Char is a punctuation mark.

punctuation_mark(Char) :- Char < 45.
punctuation_mark(Char) :- Char >= 58, Char < 63.
punctuation_mark(Char) :- Char >= 91, Char =< 96.
punctuation_mark(Char) :- Char >= 123.


% lower_case(+Char,-NewChar)
%  Translates any ASCII code to lower case.

lower_case(Char,NewChar) :-     % Add 32 to code of upper-case letter
   Char >= 65,
   Char =< 90,
   !,
   NewChar is Char+32.

lower_case(Char,Char).          % Leave all others unchanged




%Handles help requests
process([help]):-
     !,
     query(help).

% Social Gatherings ------------------------------------------------------------------------------------------
process([is,a,_,gathering,allowed,?]) :-
     !,
     readInt('How many people?',X),
     readInt('How many households?',Y),
     allowed(socialGatherings,X,Y),
     write('Your gathering with '),write(X),write(' people from up to '),write(Y),write(' household is allowed.'),nl.

process([show,is,a,_,gathering,allowed,?]) :-
     !,
     readInt('How many people?',X),
     readInt('How many households?',Y),
     showAllowed(socialGatherings,X,Y),
     write('Your gathering with '),write(X),write(' people from up to '),write(Y),write(' household is allowed.'),nl.

% Weddings ---------------------------------------------------------------------------------------------------
process([is,a,wedding,allowed,?]):-
     !,
     readInt('How many guests?',X),
     allowed(weddings,X),
     write('Your wedding with '),write(X),write(' people is allowed.'),nl.

 process([can,i,have,X,people,at,a,wedding,?]):-
     !,
     allowed(weddings,X),
     write('Your wedding with '),write(X),write(' people is allowed.'),nl.

 process([how,many,people,can,go,to,a,wedding,?]):-
     !,
     prove(restriction(weddings,(maxPeople(X)))),
     write('You can have up to '),write(X),write(' people at your wedding.'),
     nl.

process([show,is,a,wedding,allowed,?]):-
     !,
     readInt('How many guests?',X),
     showAllowed(weddings,X),
     write('Your wedding with '),write(X),write(' people is allowed.'),nl.

 process([show,can,i,have,X,people,at,a,wedding,?]):-
     !,
     showAllowed(weddings,X),
     write('Your wedding with '),write(X),write(' people is allowed.'),nl.

 process([show,how,many,people,can,go,to,a,wedding,?]):-
     !,
     show(restriction(weddings,(maxPeople(X)))),
     write('You can have up to '),write(X),write(' people at your wedding.'),
     nl.

% Indoor Events -------------------------------------------------------------------------------------------
process([how,many,people,can,go,to,indoor,events,?]):-
     !,
     prove(restriction(indoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(Y))))),
     write('You can have '),write(X),write(' people at a small venue.'),nl,
     write('You can have '),write(Y),write(' people at a large venue.'),nl.

process([can,i,have,X,people,at,a,small,indoor,event,?]):-
     allowed(indoorEvents,small,X),
     write('You can have this event!'),nl.

process([can,i,have,X,people,at,a,large,indoor,event,?]):-
     allowed(indoorEvents,large,X),
     write('You can have this event!'),nl.

process([is,a,small,indoor,event,allowed,?]):-
     readInt('How many guests?',X),
     allowed(indoorEvents,small,X),
     write('You can have this event!'),nl.

process([is,a,large,indoor,event,allowed,?]):-
     readInt('How many guests?',X),
     allowed(indoorEvents,large,X),
     write('You can have this event!'),nl.



process([show,how,many,people,can,go,to,indoor,events,?]):-
     !,
     show(restriction(indoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(Y))))),
     write('You can have '),write(X),write(' people at a small venue.'),nl,
     write('You can have '),write(Y),write(' people at a large venue.'),nl.

process([show,can,i,have,X,people,at,a,small,indoor,event,?]):-
     showAllowed(indoorEvents,small,X),
     write('You can have this event!'),nl.

process([show,can,i,have,X,people,at,a,large,indoor,event,?]):-
     showAllowed(indoorEvents,large,X),
     write('You can have this event!'),nl.

process([show,is,a,small,indoor,event,allowed,?]):-
     readInt('How many guests?',X),
     showAllowed(indoorEvents,small,X),
     write('You can have this event!'),nl.

process([show,is,a,large,indoor,event,allowed,?]):-
     readInt('How many guests?',X),
     showAllowed(indoorEvents,large,X),
     write('You can have this event!'),nl.

% Outdoor Events -------------------------------------------------------------------------------------------
process([how,many,people,can,go,to,outdoor,events,?]):-
     !,
     prove(restriction(outdoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(Y))))),
     write('You can have '),write(X),write(' people at a small venue.'),nl,
     write('You can have '),write(Y),write(' people at a large venue.'),nl.

process([can,i,have,X,people,at,a,small,outdoor,event,?]):-
     allowed(outdoorEvents,small,X),
     write('You can have this event!'),nl.

process([can,i,have,X,people,at,a,large,outdoor,event,?]):-
     allowed(outdoorEvents,large,X),
     write('You can have this event!'),nl.

process([is,a,small,outdoor,event,allowed,?]):-
     readInt('How many guests?',X),
     allowed(outdoorEvents,small,X),
     write('You can have this event!'),nl.

process([is,a,large,outdoor,event,allowed,?]):-
     readInt('How many guests?',X),
     allowed(outdoorEvents,large,X),
     write('You can have this event!'),nl.


process([show,how,many,people,can,go,to,outdoor,events,?]):-
     !,
     show(restriction(outdoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(Y))))),
     write('You can have '),write(X),write(' people at a small venue.'),nl,
     write('You can have '),write(Y),write(' people at a large venue.'),nl.

process([show,can,i,have,X,people,at,a,small,outdoor,event,?]):-
     showAllowed(outdoorEvents,small,X),
     write('You can have this event!'),nl.

process([show,can,i,have,X,people,at,a,large,outdoor,event,?]):-
     showAllowed(outdoorEvents,large,X),
     write('You can have this event!'),nl.

process([show,is,a,small,outdoor,event,allowed,?]):-
     readInt('How many guests?',X),
     showAllowed(outdoorEvents,small,X),
     write('You can have this event!'),nl.

process([show,is,a,large,outdoor,event,allowed,?]):-
     readInt('How many guests?',X),
     showAllowed(outdoorEvents,large,X),
     write('You can have this event!'),nl.

% Sports Training ----------------------------------------------------------------------------------------------
process([how,many,people,can,go,to,sports,training,?]):-
     !,
     prove(restriction(sportsTraining,(pod(indoor,maxPeople(X)) & pod(outdoor,maxPeople(Y)))) ),
     write('You can have '),write(X),write(' people at an indoor training session.'),nl,
     write('You can have '),write(Y),write(' people at an outdoor training session.'),nl.

process([can,i,have,X,people,at,indoor,training,?]):-
     allowed(sportsTraining,indoor,X),
     write('Training can go ahead!'),nl.

process([can,i,have,X,people,at,outdoor,training,?]):-
     allowed(sportsTraining,outdoor,X),
     write('Training can go ahead!'),nl.

process([is,indoor,training,allowed,?]):-
     readInt('How many guests?',X),
     allowed(sportsTraining,outdoor,X),
     write('Training can go ahead!'),nl.

process([is,outdoor,training,allowed,?]):-
     readInt('How many guests?',X),
     allowed(sportsTraining,outdoor,X),
     write('Training can go ahead!'),nl.


process([show,how,many,people,can,go,to,sports,training,?]):-
     !,
     show(restriction(sportsTraining,(pod(indoor,maxPeople(X)) & pod(outdoor,maxPeople(Y)))) ),
     write('You can have '),write(X),write(' people at an indoor training session.'),nl,
     write('You can have '),write(Y),write(' people at an outdoor training session.'),nl.

process([show,can,i,have,X,people,at,indoor,training,?]):-
     showAllowed(sportsTraining,indoor,X),
     write('Training can go ahead!'),nl.

process([show,can,i,have,X,people,at,outdoor,training,?]):-
     showAllowed(sportsTraining,outdoor,X),
     write('Training can go ahead!'),nl.

process([show,is,indoor,training,allowed,?]):-
     readInt('How many guests?',X),
     showAllowed(sportsTraining,outdoor,X),
     write('Training can go ahead!'),nl.

process([show,is,outdoor,training,allowed,?]):-
     readInt('How many guests?',X),
     showAllowed(sportsTraining,outdoor,X),
     write('Training can go ahead!'),nl.

% Matches & Events ---------------------------------------------------------------------------------------------
process([how,many,people,can,go,to,matches,?]):-
     !,
     prove(restriction(matchesAndEvents,(venue(indoor,maxPeople(X)) & venue(outdoor,maxPeople(Y)) & venue(stadia,maxPeople(Z)) & status(_) ))),     write('You can have '),write(X),write(' people at an indoor training session.'),nl,
     write('You can have '),write(X),write(' people at an indoor venue.'),
     write('You can have '),write(Y),write(' people at an outdoor venue.'),
     write('You can have '),write(Z),write(' people at a stadia venue.'),nl.

process([can,i,have,X,people,at,an,indoor,match,?]):-
     allowed(matchesAndEvents,indoor,X),
     write('Yes you can!'),nl.

process([can,i,have,X,people,at,an,outdoor,match,?]):-
     allowed(matchesAndEvents,outdoor,X),
     write('Yes you can!'),nl.

process([can,i,have,X,people,at,a,stadia,match,?]):-
     allowed(matchesAndEvents,stadia,X),
     write('Yes you can!'),nl.

process([are,indoor,matches,allowed,?]):-
     readInt('How many people?',X),
     allowed(matchesAndEvents,indoor,X),
     write('The match can go ahead!'),nl.

process([are,outdoor,matches,allowed,?]):-
     readInt('How many people?',X),
     allowed(matchesAndEvents,outdoor,X),
     write('The match can go ahead!'),nl.

process([are,stadia,matches,allowed,?]):-
     readInt('How many people?',X),
     allowed(matchesAndEvents,stadia,X),
     write('The match can go ahead!'),nl.



process([show,how,many,people,can,go,to,matches,?]):-
     !,
     show(restriction(matchesAndEvents,(venue(indoor,maxPeople(X)) & venue(outdoor,maxPeople(Y)) & venue(stadia,maxPeople(Z)) & status(_) ))),     write('You can have '),write(X),write(' people at an indoor training session.'),nl,
     write('You can have '),write(X),write(' people at an indoor venue.'),
     write('You can have '),write(Y),write(' people at an outdoor venue.'),
     write('You can have '),write(Z),write(' people at a stadia venue.'),nl.

process([show,can,i,have,X,people,at,an,indoor,match,?]):-
     showAllowed(matchesAndEvents,indoor,X),
     write('Yes you can!'),nl.

process([show,can,i,have,X,people,at,an,outdoor,match,?]):-
     showAllowed(matchesAndEvents,outdoor,X),
     write('Yes you can!'),nl.

process([show,can,i,have,X,people,at,a,stadia,match,?]):-
     showAllowed(matchesAndEvents,stadia,X),
     write('Yes you can!'),nl.

process([show,are,indoor,matches,allowed,?]):-
     readInt('How many people?',X),
     showAllowed(matchesAndEvents,indoor,X),
     write('The match can go ahead!'),nl.

process([show,are,outdoor,matches,allowed,?]):-
     readInt('How many people?',X),
     showAllowed(matchesAndEvents,outdoor,X),
     write('The match can go ahead!'),nl.

process([show,are,stadia,matches,allowed,?]):-
     readInt('How many people?',X),
     showAllowed(matchesAndEvents,stadia,X),
     write('The match can go ahead!'),nl.

% Gyms, Pools, Etc ---------------------------------------------------------------------------------------------
process([are,gyms,open,?]):-
     allowed(gymsPoolsEtc,open) ; allowed(gymsPoolsEtc,individual).

process([are,pools,open,?]):-
     process([are,gyms,open,?]).

process([can,i,go,to,the,gym,?]):-
     prove(restriction(gymsPoolsEtc,status(Status))),
     Status \= closed,
     write('Yes, you can go to the gyms, pools, etc.!'),nl.

process([can,i,go,to,the,pool,?]):-
     process([can,i,go,to,the,gym,?]).

process([what,is,the,status,of,the,gym,?]):-
     allowed(gymsPoolsEtc,Status),
     write('The status of Gyms, Pools, Etc. is '),write(Status),nl.

process([what,is,the,status,of,the,pool,?]):-
     process([what,is,the,status,of,the,gym,?]).


process([show,are,gyms,open,?]):-
     showAllowed(gymsPoolsEtc,open) ; allowed(gymsPoolsEtc,individual).

process([show,are,pools,open,?]):-
     process([show,are,gyms,open,?]).

process([show,can,i,go,to,the,gym,?]):-
     show(restriction(gymsPoolsEtc,status(Status))),
     Status \= closed,
     write('Yes, you can go to the gyms, pools, etc.!'),nl.

process([show,can,i,go,to,the,pool,?]):-
     process([show,can,i,go,to,the,gym,?]).

process([show,what,is,the,status,of,the,gym,?]):-
     showAllowed(gymsPoolsEtc,Status),
     write('The status of Gyms, Pools, Etc. is '),write(Status),nl.

process([show,what,is,the,status,of,the,pool,?]):-
     process([show,what,is,the,status,of,the,gym,?]).

% Religious Services -------------------------------------------------------------------------------------------
process([are,religious,services,allowed,?]):-
     prove(restriction(religiousServ,(service(Service) & maxPeople(X)))),
     write('The religious services available are '),write(Service),write('.'),nl,
     write('The maximum amount of people allowed to attend are '),write(X),write('.'),nl.

process([how,many,people,are,allowed,at,religious,services,?]):-
     prove(restriction(religiousServ,(service(Service) & maxPeople(X)))),
     write('The amount of people allowed to attend are '),write(X),write('.'),nl.

process([what,religious,services,are,available,?]):-
     prove(restriction(religiousServ,(service(Service) & maxPeople(X)))),
     write('The religious services available are '),write(Service),write('.'),nl.

process([can,X,people,attend,religious,services,?]):-
     allowed(religiousServ,_,X),
     write(X),write(' people can attend religious services.'),nl.


process([show,are,religious,services,allowed,?]):-
     show(restriction(religiousServ,(service(Service) & maxPeople(X)))),
     write('The religious services available are '),write(Service),write('.'),nl,
     write('The maximum amount of people allowed to attend are '),write(X),write('.'),nl.

process([show,how,many,people,are,allowed,at,religious,services,?]):-
     show(restriction(religiousServ,(service(Service) & maxPeople(X)))),
     write('The amount of people allowed to attend are '),write(X),write('.'),nl.

process([show,what,religious,services,are,available,?]):-
     show(restriction(religiousServ,(service(Service) & maxPeople(X)))),
     write('The religious services available are '),write(Service),write('.'),nl.

process([show,can,X,people,attend,religious,services,?]):-
     showAllowed(religiousServ,_,X),
     write(X),write(' people can attend religious services.'),nl.


% Bar Food Cade Etc -----------------------------------------------------------------------------
process([is,bar,food,allowed,?]):-
     readInt('How many guests?',People),
     readInt('How many households?',Households),
     allowed(barFoodCafeEtc,_,X),nl.

process([how,many,people,can,go,to,a,cafe,?]):-
     prove(restriction(barFoodCafeEtc,(dining(_) & maxPeople(X) & households(Y)))),
     write(X),write(' people from up to '),write(Y),write(' households can go to a cafe or for bar food.'),nl.

process([how,many,people,can,get,bar,food,?]):-
     process([how,many,people,can,go,to,a,cafe,?]).

process([what,is,the,status,of,cafes,?]):-
     prove(restriction(barFoodCafeEtc,(dining(Status) & maxPeople(_) & households(_)))),
     write('The status is '),write(Status),write('.'),nl.



process([show,is,bar,food,allowed,?]):-
     readInt('How many guests?',People),
     readInt('How many households?',Households),
     showAllowed(barFoodCafeEtc,_,X),nl.

process([show,how,many,people,can,go,to,a,cafe,?]):-
     show(restriction(barFoodCafeEtc,(dining(_) & maxPeople(X) & households(Y)))),
     write(X),write(' people from up to '),write(Y),write(' households can go to a cafe or for bar food.'),nl.

process([show,how,many,people,can,get,bar,food,?]):-
     process([show,how,many,people,can,go,to,a,cafe,?]).

process([show,what,is,the,status,of,cafes,?]):-
     show(restriction(barFoodCafeEtc,(dining(Status) & maxPeople(_) & households(_)))),
     write('The status is '),write(Status),write('.'),nl.

% Wet Pubs ------------------------------------------------------------------------------------
process([how,many,people,can,go,to,the,pub,?]):-
     prove(restriction(wetPubs,(dining(_) & maxPeople(X) & households(Y)))),
     (X == -42) ->
          (write('There is no restriction on the amount of people.'));
          (write(X),write(' people can go to the pub from up to '),write(Y),write(' households.')),
     nl.

process([are,pubs,open,?]):-
     prove(restriction(wetPubs,(dining(Status) & maxPeople(_) & households(_)))),
     write('The status of wet pubs is '),write(Status),write('.'),
     nl.

process([can,i,go,to,the,pub,?]):-
     readInt('How many guests?',People),
     readInt('How many households?',Households),
     allowed(wetPubs,Dining,People,Households),
     write(''),nl.



process([show,how,many,people,can,go,to,the,pub,?]):-
     show(restriction(wetPubs,(dining(_) & maxPeople(X) & households(Y)))),
     (X == -42) ->
          (write('There is no restriction on the amount of people.'));
          (write(X),write(' people can go to the pub from up to '),write(Y),write(' households.')),
     nl.

process([show,are,pubs,open,?]):-
     show(restriction(wetPubs,(dining(Status) & maxPeople(_) & households(_)))),
     write('The status of wet pubs is '),write(Status),write('.'),
     nl.

process([show,can,i,go,to,the,pub,?]):-
     readInt('How many guests?',People),
     readInt('How many households?',Households),
     showAllowed(wetPubs,Dining,People,Households),
     write(''),nl.

% Hotels Etc ----------------------------------------------------------------------
process([are,hotels,open,?]):-
     allowed(hotelsEtc,Service),
     write('The current services offered are '),write(Service),write('.'),nl.

process([show,are,hotels,open,?]):-
     showAllowed(hotelsEtc,Service),
     write('The current services offered are '),write(Service),write('.'),nl.

% Retail Services ----------------------------------------------------------------------
process([are,retail,services,open,?]):-
     allowed(retailServices,Service),
     write('The current services offered are '),write(Service),write('.'),nl.

process([show,are,retail,services,open,?]):-
     showAllowed(retailServices,Service),
     write('The current services offered are '),write(Service),write('.'),nl.

% Indoor Cultural ----------------------------------------------------------------------
process([are,indoor,cultural,services,available,?]):-
     allowed(indoorCultural,Service),
     write('The current services offered are '),write(Service),write('.'),nl.

process([show,are,indoor,cultural,services,available,?]):-
     showAllowed(indoorCultural,Service),
     write('The current services offered are '),write(Service),write('.'),nl.

% Workplace ----------------------------------------------------------------------
process([what,are,the,workplace,restrictions,?]):-
     prove(restriction(workplace,work(Work))),
     write('The current workplace restrictions offered are '),write(Work),write('.'),nl.

process([show,what,are,the,workplace,restrictions,?]):-
     show(restriction(workplace,work(Work))),
     write('The current workplace restrictions offered are '),write(Work),write('.'),nl.

% Domestic Travel ----------------------------------------------------------------------
process([what,are,the,travel,restrictions,?]):-
     prove(restriction(domesticTravel,(travel(Travel)))),
     write('The current travel restrictions are '),write(Travel),write('.'),nl.

process([am,i,allowed,to,travel,?]):-
     allowed(domesticTravel,Travel),
     write('The current travel restrictions are '),write(Travel),write('.'),nl.

process([can,i,travel,?]):-
     prove(restriction(domesticTravel,(travel(Travel)))),
     Travel \== exerciseFiveKm,
     write('Yes but the current travel restrictions are '),write(Travel),write('.'),nl.


process([show,what,are,the,travel,restrictions,?]):-
     show(restriction(domesticTravel,(travel(Travel)))),
     write('The current travel restrictions are '),write(Travel),write('.'),nl.

process([show,am,i,allowed,to,travel,?]):-
     showAllowed(domesticTravel,Travel),
     write('The current travel restrictions are '),write(Travel),write('.'),nl.

process([show,can,i,travel,?]):-
     show(restriction(domesticTravel,(travel(Travel)))),
     Travel \== exerciseFiveKm,
     write('Yes but the current travel restrictions are '),write(Travel),write('.'),nl.


% Public Transport -------------------------------------------------------------------
process([what,capacity,is,public,transport,at,?]):-
     prove(restriction(publicTransport,(capacity(X)))),
     write('The current public transport capacity is '),write(X),write('.'),nl.

process([can,i,use,public,transport,?]):-
     process([what,capacity,is,public,transport,at,?]).


process([show,what,capacity,is,public,transport,at,?]):-
     show(restriction(publicTransport,(capacity(X)))),
     write('The current public transport capacity is '),write(X),write('.'),nl.

process([show,can,i,use,public,transport,?]):-
     process([show,what,capacity,is,public,transport,at,?]).


% Schools Childcare --------------------------------------------------------------------
process([are,childcare,service,open,?]):-
     allowed(schoolsChildcare,open),
     write('Yes childcare services are open.'),nl.

process([are,schools,open,?]):-
     allowed(schoolsChildcare,open),
     write('Yes schools are open.'),nl.


process([show,are,childcare,service,open,?]):-
     showAllowed(schoolsChildcare,open),
     write('Yes childcare services are open.'),nl.

process([show,are,schools,open,?]):-
     showAllowed(schoolsChildcare,open),
     write('Yes schools are open.'),nl.


% Higher Education ------------------------------------------------------------------------
process([is,higher,education,open,?]):-
     prove(restriction(higherEdu,(status(open) & reccomendation(Rec)))),
     write('Yes and the current reccomendations are '),write(Rec),write('.'),nl.

process([are,universities,open,?]):-
     prove(restriction(higherEdu,(status(open) & reccomendation(Rec)))),
     write('Yes and the current reccomendations are '),write(Rec),write('.'),nl.

process([are,colleges,open,?]):-
     prove(restriction(higherEdu,(status(open) & reccomendation(Rec)))),
     write('Yes and the current reccomendations are '),write(Rec),write('.'),nl.

process([what,are,the,reccomendations,for,colleges,?]):-
     prove(restriction(higherEdu,(status(_) & reccomendation(Rec)))),
     write('The current reccomendations are '),write(Rec),write('.'),nl.

process([what,are,the,reccomendations,for,universities,?]):-
     prove(restriction(higherEdu,(status(_) & reccomendation(Rec)))),
     write('The current reccomendations are '),write(Rec),write('.'),nl.



process([show,is,higher,education,open,?]):-
     show(restriction(higherEdu,(status(open) & reccomendation(Rec)))),
     write('Yes and the current reccomendations are '),write(Rec),write('.'),nl.

process([show,are,universities,open,?]):-
     show(restriction(higherEdu,(status(open) & reccomendation(Rec)))),
     write('Yes and the current reccomendations are '),write(Rec),write('.'),nl.

process([show,are,colleges,open,?]):-
     show(restriction(higherEdu,(status(open) & reccomendation(Rec)))),
     write('Yes and the current reccomendations are '),write(Rec),write('.'),nl.

process([show,what,are,the,reccomendations,for,colleges,?]):-
     show(restriction(higherEdu,(status(_) & reccomendation(Rec)))),
     write('The current reccomendations are '),write(Rec),write('.'),nl.

process([show,what,are,the,reccomendations,for,universities,?]):-
     show(restriction(higherEdu,(status(_) & reccomendation(Rec)))),
     write('The current reccomendations are '),write(Rec),write('.'),nl.

% Care Homes --------------------------------------------------------------------------------------------------
process([are,care,homes,open,?]):-
     prove(restriction(careHomes,(status(open)))),
     write('They are open.'),nl.

process([are,care,homes,open,?]):-
     prove(restriction(careHomes,(status(exemptions)))),
     write('They are open with exemptions.'),nl.


process([show,are,care,homes,open,?]):-
     show(restriction(careHomes,(status(open)))),
     write('They are open.'),nl.

process([show,are,care,homes,open,?]):-
     show(restriction(careHomes,(status(exemptions)))),
     write('They are open with exemptions.'),nl.

% Over 70's ----------------------------------------------------------------------------------------------------------------
process([can,i,visit,somebody,vulnerable,?]):-
     prove(restriction(over70andVulnerable,(service(judgement)))),
     write('You should use judgement and refer to government advice.'),nl.

process([can,i,visit,someone,over,70,?]):-
     prove(restriction(over70andVulnerable,(service(judgement)))),
     write('You should use judgement and refer to government advice.'),nl.



process([show,can,i,visit,somebody,vulnerable,?]):-
     show(restriction(over70andVulnerable,(service(judgement)))),
     write('You should use judgement and refer to government advice.'),nl.

process([show,can,i,visit,someone,over,70,?]):-
     show(restriction(over70andVulnerable,(service(judgement)))),
     write('You should use judgement and refer to government advice.'),nl.

% Catch Event --------------------------------------------------------------------------------------------------
process(_) :-
     write('I\'m sorry but I dont\'t understand what you\'re asking me, please try again or ask me for help!'),
     nl.



% Helpers -------------------------------------------------------------------------------------------------------
readInt(Question,X):-
    nl,write(Question),write(' >'),read(X),integer(X).

% remove_s(+Word,-NewWord)
%  removes final S from Word, or fails if Word does not end in S.


remove_s(X,X1) :-
     name(X,XList),
     remove_s_list(XList,X1List),
     name(X1,X1List).

remove_s_list("s",[]).

remove_s_list([Head|Tail],[Head|NewTail]) :-
     remove_s_list(Tail,NewTail).


% check(+Query)
%   Try Query. Report whether it succeeded.

check(Query) :- % write('Trying query: ?- '),
                % write(Query),       % Un-comment these lines
                % nl,                 % to see the translations
                call(Query),
                !,
                write('Yes.'),
                nl.

check(_) :-     write('Not as far as I know.'),
                nl.


% note(+Fact)
%   Asserts Fact and prints acknowledgement.

note(Fact) :-  % write('Adding to knowledge base: '),
               % write(Fact),        % Un-comment these lines
               % nl,                 % to see the translations
               asserta(Fact),
               write('OK'),
               nl.
