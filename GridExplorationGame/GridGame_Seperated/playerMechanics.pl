% Player Logic 
playerLogic(stop):- nl,write('End of Game').

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
