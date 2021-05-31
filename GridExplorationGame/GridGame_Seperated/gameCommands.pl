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

% desirable items that the player does want.
desirable(gold).
desirable(chocolate).
desirable(toy).


