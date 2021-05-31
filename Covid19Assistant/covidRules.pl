:- op(1150, xfx, <- ).
:- op(950,xfy, &).

:- discontiguous allowed/4.
:- discontiguous allowed/3.
:- discontiguous allowed/2.

:- discontiguous showAllowed/4.
:- discontiguous showAllowed/3.
:- discontiguous showAllowed/2.
:- discontiguous (<-)/2.
askable(level(_)).

level(1) <- false.
level(2) <- false.
level(3) <- false.
level(4) <- false.
level(5) <- false.

%maxPeople(X) - Maximum amount of people allowed
%households(X) - Number of households allowed
%venue(X,maxPeople(Y)) - Size of venue; small, large, stadia, indoor, outdoor & max amount of people
%location(X) - Location; Indoor,Outdoor,Stadia
%pod(X,maxPeople(Y)) - X = indoor or outdoor
%status(X) - X = open, exemptions, close, individual


showAllowed(socialGatherings,People,Households):-
    show(restriction(socialGatherings,(maxPeople(X) & households(Y)))),
    X >= People,
    Y >= Households.
showAllowed(weddings,People):-
    show(restriction(weddings,(maxPeople(X)))),
    X >= People.

%showAllowed(indoorEvents/outdoorEvents,VenueSize,MaxPeople)
showAllowed(indoorEvents,small,MaxPeople):-
    show(restriction(indoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(_))))),
    X >= MaxPeople.
showAllowed(indoorEvents,large,MaxPeople):-
    show(restriction(indoorEvents,(venue(small,maxPeople(_)) & venue(large,maxPeople(X))))),
    X >= MaxPeople.

showAllowed(outdoorEvents,small,People):-
    show(restriction(outdoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(_))))),
    X >= People.
showAllowed(outdoorEvents,large,People):-
    show(restriction(outdoorEvents,(venue(small,maxPeople(_)) & venue(large,maxPeople(X))))),
    X >= People.

showAllowed(sportsTraining,indoor,People):-
    show(restriction(sportsTraining,(pod(indoor,maxPeople(X)) & pod(outdoor,maxPeople(_)))) ),
    People > 0,
    X >= People,
    nl.
showAllowed(sportsTraining,indoor,_):-
    show(restriction(sportsTraining,(pod(indoor,maxPeople(X)) & pod(outdoor,maxPeople(_)))) ),
    X == -42,
    nl.
showAllowed(sportsTraining,outdoor,People):-
    show(restriction(sportsTraining,(pod(indoor,maxPeople(_)) & pod(outdoor,maxPeople(X)))) ),
    People > 0,
    X >= People,
    nl.
showAllowed(sportsTraining,outdoor,_):-
    show(restriction(sportsTraining,(pod(indoor,maxPeople(_)) & pod(outdoor,maxPeople(X)))) ),
    X == -42,
    nl.

showAllowed(matchesAndEvents,indoor,People):-
    show(restriction(matchesAndEvents,(venue(indoor,maxPeople(X)) & venue(outdoor,maxPeople(_)) & venue(stadia,maxPeople(_)) & status(_) ))),
    X >= People,
    nl.
showAllowed(matchesAndEvents,outdoor,People):-
    show(restriction(matchesAndEvents,(venue(indoor,maxPeople(_)) & venue(outdoor,maxPeople(X)) & venue(stadia,maxPeople(_)) & status(_) ))),
    X >= People,
    nl.
showAllowed(matchesAndEvents,stadia,People):-
    show(restriction(matchesAndEvents,(venue(indoor,maxPeople(_)) & venue(outdoor,maxPeople(_)) & venue(stadia,maxPeople(X)) & status(_) ))),
    X >= People,
    nl.

showAllowed(gymsPoolsEtc,Status):-
    show(restriction(gymsPoolsEtc,status(Status))).

showAllowed(religiousServ,Service,People):-
    show(restriction(religiousServ,(service(Service) & maxPeople(X)))),
    X >= People.

showAllowed(barFoodCafeEtc,open,_,_):-
    show(restriction(barFoodCafeEtc,(dining(open) & maxPeople(X) & households(Y)))),
    X == -42,
    Y == -42.
showAllowed(barFoodCafeEtc,Dining,People,Households):-
    show(restriction(barFoodCafeEtc,(dining(Dining) & maxPeople(X) & households(Y)))),
    X >= People,
    Y >= Households.

showAllowed(wetPubs,open,_,_):-
    show(restriction(wetPubs,(dining(open) & maxPeople(X) & households(Y)))),
    X == -42,
    Y == -42.
showAllowed(wetPubs,Dining,People,Households):-
    show(restriction(wetPubs,(dining(Dining) & maxPeople(X) & households(Y)))),
    X >= People,
    Y >= Households.

showAllowed(hotelsEtc,Service):-
    show(restriction(hotelsEtc,(service(Service)))).

showAllowed(retailServices,Service):-
    show(restriction(retailServices,(service(Service)))).

showAllowed(indoorCultural,Service):-
    show(restriction(indoorCultural,(service(Service)))).

showAllowed(workplace,Work):-
    show(restriction(workplace,work(Work))).

showAllowed(domesticTravel,Travel):-
    show(restriction(domesticTravel,(travel(Travel)))).

showAllowed(publicTransport,Capacity):-
    show(restriction(publicTransport,(capacity(X)))),
    X >= Capacity.

showAllowed(schoolsChildcare,Status):-
    show(restriction(schoolsChildcare,(status(Status)))).

showAllowed(higherEdu,Status,Reccomendation):-
    show(restriction(higherEdu,(status(Status) & reccomendation(Reccomendation)))).

showAllowed(careHomes,Status):-
    show(restriction(careHomes,(status(Status)))).

showAllowed(over70andVulnerable,Service):-
    show(restriction(over70andVulnerable,(service(Service)))).






allowed(socialGatherings,People,Households):-
    prove(restriction(socialGatherings,(maxPeople(X) & households(Y)))),
    X >= People,
    Y >= Households.

allowed(weddings,People):-
    prove(restriction(weddings,(maxPeople(X)))),
    X >= People.

%allowed(indoorEvents/outdoorEvents,VenueSize,MaxPeople)
allowed(indoorEvents,small,MaxPeople):-
    prove(restriction(indoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(_))))),
    X >= MaxPeople.
allowed(indoorEvents,large,MaxPeople):-
    prove(restriction(indoorEvents,(venue(small,maxPeople(_)) & venue(large,maxPeople(X))))),
    X >= MaxPeople.

allowed(outdoorEvents,small,People):-
    prove(restriction(outdoorEvents,(venue(small,maxPeople(X)) & venue(large,maxPeople(_))))),
    X >= People.
allowed(outdoorEvents,large,People):-
    prove(restriction(outdoorEvents,(venue(small,maxPeople(_)) & venue(large,maxPeople(X))))),
    X >= People.

allowed(sportsTraining,indoor,People):-
    prove(restriction(sportsTraining,(pod(indoor,maxPeople(X)) & pod(outdoor,maxPeople(_)))) ),
    People > 0,
    X >= People,
    nl.
allowed(sportsTraining,indoor,_):-
    prove(restriction(sportsTraining,(pod(indoor,maxPeople(X)) & pod(outdoor,maxPeople(_)))) ),
    X == -42,
    nl.
allowed(sportsTraining,outdoor,People):-
    prove(restriction(sportsTraining,(pod(indoor,maxPeople(_)) & pod(outdoor,maxPeople(X)))) ),
    People > 0,
    X >= People,
    nl.
allowed(sportsTraining,outdoor,_):-
    prove(restriction(sportsTraining,(pod(indoor,maxPeople(_)) & pod(outdoor,maxPeople(X)))) ),
    X == -42,
    nl.

allowed(matchesAndEvents,indoor,People):-
    prove(restriction(matchesAndEvents,(venue(indoor,maxPeople(X)) & venue(outdoor,maxPeople(_)) & venue(stadia,maxPeople(_)) & status(_) ))),
    X >= People,
    nl.
allowed(matchesAndEvents,outdoor,People):-
    prove(restriction(matchesAndEvents,(venue(indoor,maxPeople(_)) & venue(outdoor,maxPeople(X)) & venue(stadia,maxPeople(_)) & status(_) ))),
    X >= People,
    nl.
allowed(matchesAndEvents,stadia,People):-
    prove(restriction(matchesAndEvents,(venue(indoor,maxPeople(_)) & venue(outdoor,maxPeople(_)) & venue(stadia,maxPeople(X)) & status(_) ))),
    X >= People,
    nl.

allowed(gymsPoolsEtc,Status):-
    prove(restriction(gymsPoolsEtc,status(Status))).

allowed(religiousServ,Service,People):-
    prove(restriction(religiousServ,(service(Service) & maxPeople(X)))),
    X >= People.

allowed(barFoodCafeEtc,open,_,_):-
    prove(restriction(barFoodCafeEtc,(dining(open) & maxPeople(X) & households(Y)))),
    X == -42,
    Y == -42.
allowed(barFoodCafeEtc,Dining,People,Households):-
    prove(restriction(barFoodCafeEtc,(dining(Dining) & maxPeople(X) & households(Y)))),
    X >= People,
    Y >= Households.

allowed(wetPubs,open,_,_):-
    prove(restriction(wetPubs,(dining(open) & maxPeople(X) & households(Y)))),
    X == -42,
    Y == -42.
allowed(wetPubs,Dining,People,Households):-
    prove(restriction(wetPubs,(dining(Dining) & maxPeople(X) & households(Y)))),
    X >= People,
    Y >= Households.

allowed(hotelsEtc,Service):-
    prove(restriction(hotelsEtc,(service(Service)))).

allowed(retailServices,Service):-
    prove(restriction(retailServices,(service(Service)))).

allowed(indoorCultural,Service):-
    prove(restriction(indoorCultural,(service(Service)))).

allowed(workplace,Work):-
    prove(restriction(workplace,work(Work))).

allowed(domesticTravel,Travel):-
    prove(restriction(domesticTravel,(travel(Travel)))).

allowed(publicTransport,Capacity):-
    prove(restriction(publicTransport,(capacity(X)))),
    X >= Capacity.

allowed(schoolsChildcare,Status):-
    prove(restriction(schoolsChildcare,(status(Status)))).

allowed(higherEdu,Status,Reccomendation):-
    prove(restriction(higherEdu,(status(Status) & reccomendation(Reccomendation)))).

allowed(careHomes,Status):-
    prove(restriction(careHomes,(status(Status)))).

allowed(over70andVulnerable,Service):-
    prove(restriction(over70andVulnerable,(service(Service)))).




%restriction(category,info) <- level(X)
restriction(socialGatherings,(maxPeople(10) & households(3)) ) <- level(1).
restriction(socialGatherings,(maxPeople(6) & households(3)) ) <- level(2).
restriction(socialGatherings,(maxPeople(6) & households(1)) ) <- level(3).
restriction(socialGatherings,(maxPeople(0) & households(0)) ) <- level(4).
restriction(socialGatherings,(maxPeople(0) & households(0)) ) <- level(5).

restriction(weddings,(maxPeople(100))) <- level(1).
restriction(weddings,(maxPeople(50))) <- level(2).
restriction(weddings,(maxPeople(25))) <- level(3).
restriction(weddings,(maxPeople(6))) <- level(4).
restriction(weddings,(maxPeople(6))) <- level(5).

restriction(indoorEvents,(venue(small,maxPeople(100)) & venue(large,maxPeople(200)))) <- level(1).
restriction(indoorEvents,(venue(small,maxPeople(50)) & venue(large,maxPeople(100)))) <- level(2).
restriction(indoorEvents,(venue(small,maxPeople(0)) & venue(large,maxPeople(0)))) <- level(3).
restriction(indoorEvents,(venue(small,maxPeople(0)) & venue(large,maxPeople(0)))) <- level(4).
restriction(indoorEvents,(venue(small,maxPeople(0)) & venue(large,maxPeople(0)))) <- level(5).

restriction(outdoorEvents,(venue(small,maxPeople(200)) & venue(large,maxPeople(500)))) <- level(1).
restriction(outdoorEvents,(venue(small,maxPeople(100)) & venue(large,maxPeople(200)))) <- level(2).
restriction(outdoorEvents,(venue(small,maxPeople(15)) & venue(large,maxPeople(15)))) <- level(3).
restriction(outdoorEvents,(venue(small,maxPeople(15)) & venue(large,maxPeople(15)))) <- level(4).
restriction(outdoorEvents,(venue(small,maxPeople(0)) & venue(large,maxPeople(0)))) <- level(5).

restriction(sportsTraining,(pod(indoor,maxPeople(-42)) & pod(outdoor,maxPeople(-42))) ) <- level(1).
restriction(sportsTraining,(pod(indoor,maxPeople(6)) & pod(outdoor,maxPeople(15)))  ) <- level(2).
restriction(sportsTraining,(pod(indoor,maxPeople(1)) & pod(outdoor,maxPeople(15)))  ) <- level(3).
restriction(sportsTraining,(pod(indoor,maxPeople(1)) & pod(outdoor,maxPeople(15)))  ) <- level(4).
restriction(sportsTraining,(pod(indoor,maxPeople(1)) & pod(outdoor,maxPeople(1)))  ) <- level(5).

restriction(matchesAndEvents,(venue(indoor,maxPeople(100)) & venue(outdoor,maxPeople(200)) & venue(stadia,maxPeople(500)) & status(open) ) ) <- level(1).
restriction(matchesAndEvents,(venue(indoor,maxPeople(50)) & venue(outdoor,maxPeople(100)) & venue(stadia,maxPeople(200)) & status(open) ) ) <- level(2).
restriction(matchesAndEvents,(venue(indoor,maxPeople(0)) & venue(outdoor,maxPeople(0)) & venue(stadia,maxPeople(0)) & status(exemptions) ) ) <- level(3).
restriction(matchesAndEvents,(venue(indoor,maxPeople(0)) & venue(outdoor,maxPeople(0)) & venue(stadia,maxPeople(0)) & status(exemptions) ) ) <- level(4).
restriction(matchesAndEvents,(venue(indoor,maxPeople(0)) & venue(outdoor,maxPeople(0)) & venue(stadia,maxPeople(0)) & status(closed) ) ) <- level(5).

restriction(gymsPoolsEtc,status(open)) <- level(1).
restriction(gymsPoolsEtc,status(open)) <- level(2).
restriction(gymsPoolsEtc,status(individual)) <- level(3).
restriction(gymsPoolsEtc,status(closed)) <- level(4).
restriction(gymsPoolsEtc,status(closed)) <- level(5).

restriction(religiousServ,(service(all) & maxPeople(50))) <- level(1).
restriction(religiousServ,(service(all) & maxPeople(50))) <- level(2).
restriction(religiousServ,(service(funeral) & maxPeople(25))) <- level(3).
restriction(religiousServ,(service(funeral) & maxPeople(25))) <- level(4).
restriction(religiousServ,(service(funeral) & maxPeople(10))) <- level(5).

restriction(barFoodCafeEtc,(dining(open) & maxPeople(-42) & households(-42))) <- level(1).
restriction(barFoodCafeEtc,(dining(open) & maxPeople(6) & households(3))) <- level(2).
restriction(barFoodCafeEtc,(dining(restrictions) & maxPeople(6) & households(3))) <- level(3).
restriction(barFoodCafeEtc,(dining(outdoor) & maxPeople(15) & households(3))) <- level(4).
restriction(barFoodCafeEtc,(dining(takeaway) & maxPeople(0) & households(0))) <- level(5).

restriction(wetPubs,(dining(open) & maxPeople(-42) & households(-42))) <- level(1).
restriction(wetPubs,(dining(open) & maxPeople(6) & households(3))) <- level(2).
restriction(wetPubs,(dining(restrictions) & maxPeople(6) & households(3))) <- level(3).
restriction(wetPubs,(dining(outdoor) & maxPeople(15) & households(3))) <- level(4).
restriction(wetPubs,(dining(takeaway) & maxPeople(0) & households(0))) <- level(5).

restriction(hotelsEtc,(service(open))) <- level(1).
restriction(hotelsEtc,(service(open))) <- level(2).
restriction(hotelsEtc,(service(residents))) <- level(3).
restriction(hotelsEtc,(service(existing))) <- level(4).
restriction(hotelsEtc,(service(essential))) <- level(5).

restriction(retailServices,(service(open))) <- level(1).
restriction(retailServices,(service(open))) <- level(2).
restriction(retailServices,(service(open))) <- level(3).
restriction(retailServices,(service(essential))) <- level(4).
restriction(retailServices,(service(essential))) <- level(5).

restriction(indoorCultural,(service(open)))<- level(1).
restriction(indoorCultural,(service(open)))<- level(2).
restriction(indoorCultural,(service(collect)))<- level(3).
restriction(indoorCultural,(service(online)))<- level(4).
restriction(indoorCultural,(service(online)))<- level(5).

restriction(workplace,work(staggered)) <- level(1).
restriction(workplace,work(exemptions)) <- level(2).
restriction(workplace,work(necessary)) <- level(3).
restriction(workplace,work(essential)) <- level(4).
restriction(workplace,work(essential)) <- level(5).

restriction(domesticTravel,(travel(free))) <- level(1).
restriction(domesticTravel,(travel(free)))<- level(2).
restriction(domesticTravel,(travel(county)))<- level(3).
restriction(domesticTravel,(travel(essential)))<- level(4).
restriction(domesticTravel,(travel(exerciseFiveKm)))<- level(5).

restriction(publicTransport,(capacity(100))) <- level(1).
restriction(publicTransport,(capacity(50))) <- level(2).
restriction(publicTransport,(capacity(50))) <- level(3).
restriction(publicTransport,(capacity(25))) <- level(4).
restriction(publicTransport,(capacity(25))) <- level(5).

restriction(schoolsChildcare,(status(open))) <- level(1).
restriction(schoolsChildcare,(status(open))) <- level(2).
restriction(schoolsChildcare,(status(open))) <- level(3).
restriction(schoolsChildcare,(status(open))) <- level(4).
restriction(schoolsChildcare,(status(exemptions))) <- level(5).

restriction(higherEdu,(status(open) & reccomendation(offPeak))) <- level(1).
restriction(higherEdu,(status(open) & reccomendation(peakReserved))) <- level(2).
restriction(higherEdu,(status(limited) & reccomendation(necessary))) <- level(3).
restriction(higherEdu,(status(essential) & reccomendation(avoid))) <- level(4).
restriction(higherEdu,(status(online) & reccomendation(avoid))) <- level(5).

restriction(careHomes,(status(open))) <- level(1).
restriction(careHomes,(status(open)))<- level(2).
restriction(careHomes,(status(exemptions))) <- level(3).
restriction(careHomes,(status(exemptions))) <- level(4).
restriction(careHomes,(status(exemptions))) <- level(5).

restriction(over70andVulnerable,(service(judgement))) <- level(_).
