:- ensure_loaded('gameMechanics.pl').
:- ensure_loaded('gameCommands.pl').

%Startup call
start :- 
    write('Grid Game'),nl,
    write('Startup... (This could take a minute)'),nl,
    gameSetup,game_loop.

game_loop :-
    printGrid,nl,write('Type help to view other commands.'),nl,
    write('Please choose a direction to move(north/south/east/west)'),
    read(Direction),playerLogic(Direction).



writeInventory:-
    write('Inventory List:'),nl,
    forall(inventory(Symbol,_,_),
           (write('- '),itemToSymbol(Item,Symbol),write(Item),nl)
          ).




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

checkSurroundings(Value):-
    cell(X,Y,Value),
    nl,write('Player Coords:'),write(X),write(','),write(Y),nl,
    checkDirectionForItem(X,Y,northWest),
    checkDirectionForItem(X,Y,north),
    checkDirectionForItem(X,Y,northEast),
    checkDirectionForItem(X,Y,west),
    checkDirectionForItem(X,Y,east),
    checkDirectionForItem(X,Y,southWest),
    checkDirectionForItem(X,Y,south),
    checkDirectionForItem(X,Y,southEast).




checkDirection(X,Y,Direction):-
    (   direction(Direction) -> (directionToMovement(Direction,AddX,AddY), NewX is X + AddX, NewY is Y + AddY,(checkBounds(NewX,NewY) -> writeCellDir(NewX,NewY,Direction);
     writeDirFail(X,Y,Direction) ));true).

checkDirectionForItem(X,Y,Direction):-
    (direction(Direction) ->
    (directionToMovement(Direction,AddX,AddY),
     NewX is X + AddX,
     NewY is Y + AddY,
    (checkBounds(NewX,NewY) ->
        doesCellHaveItemPrint(NewX,NewY,Direction)
    ); true)
    ).

doesCellHaveItemPrint(X,Y,Direction):-
    cell(X,Y,Value),
    itemToSymbol(Item,Value),
    (item(Item) -> writeCellDir(X,Y,Direction);true).

doesCellHaveItem(X,Y):-
    cell(X,Y,Value),
    itemToSymbol(Item,Value),
    item(Item).



writeCellDir(X,Y,Direction):-
    cell(X,Y,Value),
    write(Direction),
    write(':  Value:'),write(Value),
    write(' - Coords:'),write(X ),write(',' ),write(Y),nl.

writeDirFail(X,Y,Direction):-
    write(Direction),
    write(':  Out of bounds, not on grid.'),
    write(' - Coords:'),write(X ),write(',' ),write(Y),nl.

% ENDREGION - Grid Cell Methods

% Player Logic 
playerLogic(stop):- nl,write('End of Game').

playerLogic(help):- nl,write('Commands:'),nl,write('inventory. - displays the players inventory'),write('mapLegend. - find out what symbols mean on map.'),nl,game_loop.

playerLogic(inventory):- nl, writeInventory,nl,game_loop.

playerLogic(mapLegend):- nl, write('Legend:'),nl,write('p - Player'),nl,write('g - Gold (Desirable Item)'),nl,write('c - Chocolate (Desirable Item)'),write('h - Homework (Undesirable Item)'),nl,game_loop.

playerLogic(Direction) :-
    direction(Direction), moveableDirection(Direction),
    write('Moving '),write(Direction),nl,
    directionToMovement(Direction,X,Y),
    cell(OldX,OldY,player),
    NewX is OldX + X,
    NewY is OldY + Y,
    checkBounds(NewX,NewY) ->(movePlayer(OldX,OldY,NewX,NewY),game_loop); (write('Invalid Move.'),nl,game_loop).

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

% ENDREGION - Player Logic

% REGION - Item Pickup/Drop Logic 

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

% ENDREGION - Item Drop Logic