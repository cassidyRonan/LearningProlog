%Dynamic Predicate Declarations
:- dynamic globalVal/2. %my version of nb_setval and nb_getval because they did not work reliabily even in singular tests - globalVal(name,value)
:- dynamic cell/3. % Cell value of grid - cell(X,Y,Value)
:- dynamic inventory/3. % inventory(itemName,X,Y). where X,Y correspond to location it was found
:- dynamic listVal/2.

%Game Command Declarations
% direction that are accepted as input
direction(north).
direction(south).
direction(east).
direction(west).
direction(northEast).
direction(northWest).
direction(southEast).
direction(southWest).

% directions which the player can move
moveableDirection(north).
moveableDirection(south).
moveableDirection(east).
moveableDirection(west).

%directionToMovement(direction,X Movement Value, Y Movement Value).
directionToMovement(north,0,1).
directionToMovement(south,0,-1).
directionToMovement(east,1,0).
directionToMovement(west,-1,0).
directionToMovement(northEast,1,1).
directionToMovement(northWest,-1,1).
directionToMovement(southEast,1,-1).
directionToMovement(southWest,-1,-1).

% items that can be found on the map
item(gold).
item(chocolate).
item(toy).
item(sock).
item(homework).
item(almonds).

% item to symbol converter for map - itemToSymbol(itemName,symbol)
itemToSymbol(gold,g).
itemToSymbol(chocolate,c).
itemToSymbol(toy,t).
itemToSymbol(sock,s).
itemToSymbol(homework,h).
itemToSymbol(almonds,a).

% desirable items that the player does want, if an item is not desirable then it is undesirable
desirable(gold).
desirable(chocolate).
desirable(toy).




%Logic and Action
start :-
    write('Grid Game'),nl,
    write('Startup... (This could take a minute)'),nl,
    gameSetup,game_loop.

game_loop :-
    printGrid,nl,write('Type help to view other commands.'),nl,
    write('Please choose a direction to move(north/south/east/west)'),
    read(Direction),playerLogic(Direction).

resume:-
    game_loop.

grid(Size):-  % Sets grid to be a square of length and width "Size"
    grid(Size,Size).

grid(_,0):- %End of grid loop catch when row == 0
    nl.

% Creates the grid for the player to navigate
grid(XSpan,YSpan):-
    (XSpan>0,YSpan>0) -> % check to ensure that grid doesn't go into negative values
    (createColumn(XSpan,YSpan),
    NextRow is YSpan - 1,
    grid(XSpan,NextRow))
    ; write('Grid creation error, ngative values detected.').

% End of create column loop to prevent negative values
createColumn(1,Row):-
    assertz(cell(1,Row,empty)).

% Adds an empty cell to the end of the database since we are using assertz, moves onto the next column in specfied row
createColumn(Column,Row):-
    assertz(cell(Column,Row,empty)),
    NextCol is Column-1,
    createColumn(NextCol,Row).


% Removes value from cell that matches X and Y coords then places new value in.
placeInGrid(Value,X,Y):-
    retract(cell(X,Y,_)),
    assertz(cell(X,Y,Value)).

placeRandom(Value):-
    globalVal(gridSize,S),
    random_between(1,S,X),
    random_between(1,S,Y),
    placeInGrid(Value,X,Y).

checkBounds(X,Y):- %Checks to see if X,Y coords are within the bounds of the grid.
    globalVal(gridSize,S),!,
    %maxGrid(Xm,Ym),
    BoundX is S + 1,
    BoundY is S + 1,
    (X > 0,X < BoundX),(Y > 0,Y < BoundY).

maxRow(Y):- %Bag of not grabbing highest value for some reason, grabs 2 and 1 not 3
    bagof([R],cell(_,R,_),L),!,
    max_list(L,Y).

maxCol(X):-
    bagof([R],cell(R,_,_),L),!,
    max_list(L,X).

maxGrid(X,Y):-
    maxCol(X),maxRow(Y).

% Game setup method, default is 3x3 grid
gameSetup:-
    gameSetup(3).

gameSetup(Size):-
    retractall(cell(_,_,_)), %Clearing previous grid data amd inventory data to avoid duplication of data
    retractall(inventory(_,_,_)),
    %b_setval(gridSize,Size), % Was previously using built in predicate but had many issues with it, hence creation of globalVar
    setGlobal(gridSize,Size),
    grid(Size),!,
    placeRandom(g),!,
    placeRandom(c),!,
    placeRandom(h),
    placeInGrid(player,2,2). %could look into dynamically setting position in future but not required for assignment
 %undesirable item


printGrid:- % Default print for 3x3 square starts at 1 in X to build the array from left to right similar to X,Y axis view
    printGrid(1,3).

printGrid(_,0):- nl.

printGrid(Column,Row):- %Works fairly similaryly to the creation of the grid except it prints the data instead of creating it.
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


writeCellDir(X,Y,Direction):-
    cell(X,Y,Value),
    write(Direction),
    write(':  Value:'),write(Value),
    write(' - Coords:'),write(X ),write(',' ),write(Y),nl.

writeDirFail(X,Y,Direction):-
    write(Direction),
    write(':  Out of bounds, not on grid.'),
    write(' - Coords:'),write(X ),write(',' ),write(Y),nl.

% REGION - Grid Cell Methods
getAdjacent(Value):-
    cell(X,Y,Value),
    nl,write('Player Coords:'),write(X),write(','),write(Y),nl,
    checkDirection(X,Y,north),
    checkDirection(X,Y,south),
    checkDirection(X,Y,east),
    checkDirection(X,Y,west).

getSurrounding(Value):-
    cell(X,Y,Value),
    nl,write('Player Coords:'),write(X),write(','),write(Y),nl,
    checkDirection(X,Y,northWest),
    checkDirection(X,Y,north),
    checkDirection(X,Y,northEast),
    checkDirection(X,Y,west),
    checkDirection(X,Y,east),
    checkDirection(X,Y,southWest),
    checkDirection(X,Y,south),
    checkDirection(X,Y,southEast).

playerSurroundings(List):-
    checkSurroundings(player,List).

checkSurroundings(Value,List):-
    retractall(listVal(surrounding,_)),
    cell(X,Y,Value),
    nl,write('Player Coords:'),write(X),write(','),write(Y),nl,
    checkDirectionForItem(X,Y,northWest),
    checkDirectionForItem(X,Y,north),
    checkDirectionForItem(X,Y,northEast),
    checkDirectionForItem(X,Y,west),
    checkDirectionForItem(X,Y,east),
    checkDirectionForItem(X,Y,southWest),
    checkDirectionForItem(X,Y,south),
    checkDirectionForItem(X,Y,southEast),
    bagof(Item,listVal(surrounding,Item),List).


checkDirection(X,Y,Direction):-
    (   direction(Direction) -> (directionToMovement(Direction,AddX,AddY), NewX is X + AddX, NewY is Y + AddY,(checkBounds(NewX,NewY) -> writeCellDir(NewX,NewY,Direction);
     writeDirFail(X,Y,Direction) ));true).

checkDirectionForItem(X,Y,Direction):-
    (direction(Direction) ->
        (directionToMovement(Direction,AddX,AddY),
         NewX is X + AddX,
         NewY is Y + AddY,!,
        (checkBounds(NewX,NewY) ->
            (doesCellHaveItem(NewX,NewY) ->
               (assertz(listVal(surrounding,[NewX,NewY])));true)
         );
     true)
 ;true).




matches(Value,X):- (Value == X).

setGlobal(Name,Value):-
    retractall(globalVal(Name,_)),
    assertz(globalVal(Name,Value)).


%Item related logic
checkItemOnGround(Symbol):- %Checks desirability of specified item through use of symbol
    checkDesirability(Symbol) -> true;false.


checkDesirability(Symbol):-
    itemToSymbol(Item,Symbol),
    item(Item),
    desirable(Item).

doesCellHaveItemPrint(X,Y,Direction):- %Prints cell data if the cell does have an item
    cell(X,Y,Value),
    itemToSymbol(Item,Value),
    (item(Item) -> writeCellDir(X,Y,Direction);true).

doesCellHaveItem(X,Y):-
    cell(X,Y,Value),
    itemToSymbol(Item,Value),
    item(Item).

pickupItem(Symbol,X,Y):-
    assertz(inventory(Symbol,X,Y)),
    itemToSymbol(Item,Symbol),
    write(Item),write(' has been added to your inventory.'),nl.

dropItem:-
    inventory(_,_,_) -> dropLogic;true.

dropLogic:-
    write('Oh no, you found an undesirable item! You must drop one of your items!'),nl,
    writeInventory,
    write('Choose an item to discard:'),
    read(Input),nl,
    itemToSymbol(Input,Sym),
    dropInputCheck(Sym).

dropInputCheck(Input):-
    inventory(Input,X,Y) -> (placeInGrid(Input,X,Y),retract(inventory(Input,X,Y)),write('The item has been returned to its original location!'),nl);(write('Invalid Seletion.'),nl,dropLogic).



% Player Logic
playerLogic(stop):- nl,write('End of Game').

playerLogic(pause):-
    nl,write('Paused Game Loop'),nl,write('Type resume to hop back into the game.'),nl,nl.

playerLogic(help):-
    nl,write('Commands:'),
    nl,write('inventory. - Displays the players inventory'),
    nl,write('mapLegend. - Find out what symbols mean on map.'),
    nl,write('playerSurroundings(List). - Must pause loop before use; Returns list of surrounding cells that have items.'),
    nl,write('pause. - pauses the game loop so playerSurroundings can be used.'),
    nl,nl,game_loop.

playerLogic(inventory):-
    nl, writeInventory,
    nl,game_loop.

playerLogic(mapLegend):-
    nl, write('Legend:'),
    nl,write('p - Player'),
    nl,write('g - Gold (Desirable Item)'),
    nl,write('c - Chocolate (Desirable Item)'),
    nl,write('h - Homework (Undesirable Item)'),
    nl,game_loop.

playerLogic(Direction) :-
    direction(Direction), moveableDirection(Direction),
    write('Moving '),write(Direction),
    nl, directionToMovement(Direction,X,Y),
    cell(OldX,OldY,player),
    NewX is OldX + X,
    NewY is OldY + Y,
    checkBounds(NewX,NewY) ->
        (movePlayer(OldX,OldY,NewX,NewY),game_loop);
        (write('Invalid Move.'),nl,game_loop).

playerLogic(Direction) :-
    \+ (direction(Direction),moveableDirection(Direction)),
    write('Invalid Direction. Try Again.'),nl,
    game_loop.

movePlayer(OldX,OldY,NewX,NewY):-
    placeInGrid(empty,OldX,OldY),
    (doesCellHaveItem(NewX,NewY) ->
        (cell(NewX,NewY,Val) ->
            (checkDesirability(Val) ->
                pickupItem(Val,NewX,NewY);dropItem)
         ;true)
    ;true),
    placeInGrid(player,NewX,NewY).


writeInventory:-
    write('Inventory List:'),nl,
    forall(inventory(Symbol,_,_),
           (write('- '),itemToSymbol(Item,Symbol),write(Item),nl)
          ).

% ENDREGION - Player Logic
