:- dynamic globalVal/2. %my version of nb_setval and nb_getval because they did not work reliabily even in singular tests - globalVal(name,value)
:- dynamic cell/3. % Cell value of grid - cell(X,Y,Value)

:- dynamic inventory/3. % inventory(itemName,X,Y). where X,Y correspond to location it was found

% REGION - grid setup methods
grid(Size):- grid(Size,Size).
grid(_,0):-nl.
grid(XSpan,YSpan):-
    XSpan>0,YSpan>0,
    createColumn(XSpan,YSpan),
    NextRow is YSpan - 1,
    grid(XSpan,NextRow).

createColumn(1,Row):- assertz(cell(1,Row,empty)).
createColumn(Column,Row):-
    assertz(cell(Column,Row,empty)),
    NextCol is Column-1,
    createColumn(NextCol,Row).
% ENDREGION - End of grid setup methods

% REGION - Value placement methods for use in grid
placeInGrid(Value,X,Y):-
    retract(cell(X,Y,_)),
    assertz(cell(X,Y,Value)).

placeRandom(Value):-
    globalVal(gridSize,S),
    X is random(S + 1), (matches(0,X) -> Xc is 1;Xc is X),
    Y is random(S + 1), (matches(0,Y) -> Yc is 1;Yc is Y),
    cell(Xc,Yc,Prev),
    (checkBounds(Xc,Yc),matches(Prev,empty)) -> (placeInGrid(Value,Xc,Yc);placeRandom(Value)).
% ENDREGION - Value placement methods for use in grid

% REGION - Game setup methods, default is 3x3 grid
gameSetup:-
    gameSetup(3).

gameSetup(Size):- %dynamic sized game
    retractall(cell(_,_,_)),
    retractall(inventory(_,_,_)),
    %b_setval(gridSize,Size),
    setGlobal(gridSize,Size),
    grid(Size),
    placeInGrid(player,2,2), %could look into dynamically setting position in future but not required for assignment
    placeRandom(g),
    placeRandom(c),
    placeRandom(h), %undesirable item
    true.
% ENDREGION - Game setup methods, default is 3x3 grid

% REGION - Print methods for grid(map) display
printGrid:-
    printGrid(1,3).
printGrid(_,0):- nl.
printGrid(Column,Row):-
    printColumn(Column,Row),nl,
    NextRow is Row - 1,
    (checkBounds(Column,NextRow) -> printGrid(Column,NextRow);true).


printColumn(3,Row):- cell(3,Row,Value), printValue(Value).

printColumn(CurrentColumn,Row):-
    cell(CurrentColumn,Row,Value),
    printValue(Value),
    NextCol is CurrentColumn + 1,
   (checkBounds(NextCol,Row) -> printColumn(NextCol,Row);true).

printValue(Value):-
(matches(Value,empty)-> write('-'); (matches(Value,player) -> write('p');write(Value)) ).

% ENDREGION - Print methods for grid(map) display

matches(Value,X):- (Value == X).


setGlobal(Name,Value):-
    retractall(globalVal(Name,_)),
    assertz(globalVal(Name,Value)).

checkItemOnGround(Symbol):-
    checkDesirability(Symbol) -> true;false.


checkDesirability(Symbol):-
    itemToSymbol(Item,Symbol),
    item(Item),
    desirable(Item).

checkBounds(X,Y):-
    %b_getval(gridSize,S),
    %b_setval(gridSize,S),
    globalVal(gridSize,S),
    Bound is S + 1,
    (X > 0,X < Bound),(Y > 0,Y < Bound).
