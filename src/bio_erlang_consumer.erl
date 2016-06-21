%%% The module for consuming sample data for alignment.
%%%
%%% This module takes two strings and a setup, generates its alignment,
%%% writes out to standard output and notify the handler process. 
-module(bio_erlang_consumer).
-export([consume/2]).

%% Returns atom ok after consume the data.
%% 1st param -> Handler process' ID.
%% 2nd param -> 3-ple with two strings to be aligned and setup for alignment.
consume(HandlerPID, {String1, String2, Setup}) ->
    {S1, S2} = bio_erlang_aligner:align(String1, String2, Setup),
    io:format("~n~s~n~s~n~n", [S1, S2]),
    HandlerPID ! {consumer, ok},
    ok.