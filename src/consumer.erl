-module(consumer).
-export([consume/2]).
-compile(nucleic_acid).

consume(TaskHandlerPID, {String1, String2, Setup}) ->
    {S1, S2} = nucleic_acid:align(String1, String2, Setup),
    io:format("~n~s~n~s~n~n", [S1, S2]),
    TaskHandlerPID ! {producer, ok}.
