% `<-' is the object-level `if' - it is an infix meta-level predicate
:- op(1150, xfx, <- ).

% `&' is the object level conjunction.
% It is an infix meta-level binary function symbol:
:- op(950,xfy, &).

% issue(evidence,cause,resolutions).
issue(badAirFilter,dirtyAirFilter,replaceAirFilter).
issue(wornSparkPlugs,oldDirtySparkPlugs,cleanReplaceSparkPlugs).
issue(wornIgnitionWires,badIgnitionWires,replaceIgnitionWires).
issue(waterInGasoline,irregularFillingOfGasTank,drainGasTankAndRefill).

issue(fuelFilterClogged,badFuelFilter,replaceFuelFilter).
issue(catalyticConverterClogged,badCatalyticConverter,replaceCatalyticConverter).
issue(carburettorChokeUnset,badCarburettorChoke,checkChokePlateAndOpen).
issue(engineOverheating,faultyCoolingSystem,repairCoolingSystem).

issue(lowFuelPressure,badFuelPressureRegulator,replaceFuelPressureRegulator).
issue(ignitionTimingIncorrect,irregularIgnitionTiming,adjustIgnitionTiming).
issue(vacuumLeakage,crackedVacuum,replaceVacuumLines).
issue(egrValveOpen,badEGRValve,replaceEGRValve).

issue(dirtyFuelInjector,badFuelInjectors,replaceFuelInjectors).
issue(exhaustSystemPlugged,badExhaustSystem,replaceExhaustSystem).
issue(vacuumDisconnected,badVacuumLines,replaceVacuumLines).
issue(lowPowerSteering,badPowerSteeringFluid,refillPowerSteeringFluid).

issue(badAlternatorBearings,oldAltenatorBearings,replaceAlternator).
issue(badWaterPump,oldWaterPump,replaceWaterPump).
issue(badPowerSteeringPump,oldPowerSteerngPump,replacePowerSteeringPump).
issue(badAirConditioningCompressor,oldAirConditioningCompressor,replaceAirConditioning).

issue(leakageFuelLines,crackedFuelLines,replaceFuelLines).
issue(leakingFuelInjectors,crackedFuelInjectors,replaceInjectors).
issue(badGasCap,oldGasCap,replaceGasCap).
issue(badFuelPump,oldFuelPump,replaceFuelPump).

issue(slippedCamshaftTimingBelt,badTimingBelt,replaceTimingBelt).
issue(brokenValveAndCamshaft,badValveAndCamshaft,replaceValveAndCamshaft).
issue(misplacedSparkPlugWires,incompatibleSparkPlugs,checkFiringOrderAndPlaceOnCorrectPlugs).
issue(glazedDistributorCapOrRotor,rotorOverheating,cleanSandTheRotor).

issue(valvesNeedAdjustment,noValvesAdjusted,checkAdjustValves).
issue(lowOilPressure,oldOilPump,checkReplaceOilPump).
issue(badHydraulicValveLifters,oldHydraulicValveLifters,replaceValveLifters).
issue(wornPushRods,badPushRods,replacePushRods).

issue(sludgeInEngine,oilFlowRestricitionAndBadFilter,replaceOilfFilterAndFill).
issue(stuckEngineValves,oldEngineValves,repairEngineValves).
issue(cloggedAirFilter,badAirFilter,replaceAirFilter).


%problem(name,evidence)
problem(engineHesitates,badAirFilter).
problem(engineHesitates,wornSparkPlugs).
problem(engineHesitates,wornIgnitionWires).
problem(engineHesitates,waterInGasoline).
problem(engineHesitates,fuelFilterClogged).
problem(engineHesitates,catalyticConverterClogged).

problem(engineMisfire,carburettorChokeUnset).
problem(engineMisfire,engineOverheating).
problem(engineMisfire,lowFuelPressure).
problem(engineMisfire,ignitionTimingIncorrect).
problem(engineMisfire,fuelFilterClogged).
problem(engineMisfire,vacuumLeakage).
problem(engineMisfire,egrValveOpen).
problem(engineMisfire,dirtyFuelInjector).

problem(engineHissing,engineOverheating).
problem(engineHissing,exhaustSystemPlugged).
problem(engineHissing,vacuumDisconnected).
problem(engineHissing,vacuumLeakage).

problem(whirringSound,lowPowerSteering).
problem(whirringSound,badAlternatorBearings).
problem(whirringSound,badWaterPump).
problem(whirringSound,badPowerSteeringPump).
problem(whirringSound,badAirConditioningCompressor).

problem(strongGasOdour,leakageFuelLines).
problem(strongGasOdour,leakingFuelInjectors).
problem(strongGasOdour,badGasCap).
problem(strongGasOdour,lowFuelPressure).

problem(noIncreaseInSpeed,badAirFilter).
problem(noIncreaseInSpeed,cloggedAirFilter).
problem(noIncreaseInSpeed,ignitionTimingIncorrect).
problem(noIncreaseInSpeed,catalyticConverterClogged).
problem(noIncreaseInSpeed,waterInGasoline).
problem(noIncreaseInSpeed,badFuelPump).

problem(engineBackfire,slippedCamshaftTimingBelt).
problem(engineBackfire,ignitionTimingIncorrect).
problem(engineBackfire,brokenValveAndCamshaft).
problem(engineBackfire,misplacedSparkPlugWires).

problem(poppingSound,badAirFilter).
problem(poppingSound,wornIgnitionWires).
problem(poppingSound,glazedDistributorCapOrRotor).

problem(tappingNoise,valvesNeedAdjustment).
problem(tappingNoise,lowOilPressure).
problem(tappingNoise,badHydraulicValveLifters).
problem(tappingNoise,wornPushRods).

problem(tickingNoise,valvesNeedAdjustment).
problem(tickingNoise,sludgeInEngine).
problem(tickingNoise,badHydraulicValveLifters).
problem(tickingNoise,stuckEngineValves).
problem(tickingNoise,wornPushRods).

start:- nl,query.

query:-
    write('Problems:'),nl,
    write('1. Engine Hesitates'),nl,
    write('2. Engine Surge or Misfire'),nl,
    write('3. Hissing Sound From Engine'),nl,
    write('4. Whirring Sound From Engine That Gets Worse As Speed Increases'),nl,
    write('5. Engine using more fuel and strong gas odour'),nl,
    write('6. Engine does not want to increase its speed'),nl,
    write('7. Engine backfires when gas pedal pressed'),nl,
    write('8. Engine hesitates and popping heard from engine'),nl,
    write('9. Engine makes tapping noise when idle'),nl,
    write('10.Engine makes ticking noise'),nl,
    write('Please enter the number of your problem: '),read(Choice),nl,logic(Choice).

logic(1):-
    listIssues(engineHesitates,1).

logic(2):-
   listIssues(engineMisfire,2).

logic(3):-
    listIssues(engineHissing,3).

logic(4):-
    listIssues(whirringSound,4).

logic(5):-
    listIssues(strongGasOdour,5).

logic(6):-
    listIssues(noIncreaseInSpeed,6).

logic(7):-
    listIssues(engineBackfire,7).

logic(8):-
    listIssues(poppingSound,8).

logic(9):-
    listIssues(tappingNoise,9).

logic(10):-
    listIssues(tickingNoise,10).


listIssues(ProblemType,X):-
    write('Possible Issues:'),nl,
    bagof(Evidence,problem(ProblemType,Evidence),EvidenceList),
    writeList(EvidenceList),
    getIssue,
    repeatCheck('Want to check a different issue?') -> logic(X).

getIssue:-
    write('Please enter issue name: '),read(IssueName),nl,
    issue(IssueName,Cause,Resolution),
    write('The cause of this is likely '),write(Cause),nl,
    write('A resolution to this is '),write(Resolution),nl.

repeatCheck(y):- true.
repeatCheck(n):- !,false.
repeatCheck(Message):- nl,write(Message),write(' (y,n)'),read(Ans),nl,repeatCheck(Ans).

writeList([]):- nl.
writeList([H|T]):-
    write(H),nl,writeList(T).
