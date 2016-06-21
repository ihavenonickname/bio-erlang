%%% The module for producing sample data for alignment.
%%%
%%% This module generates two random strings and a setup. 
%%% Setup is a 3-ple with:
%%% 1) Value for a match
%%% 2) Value for a mismatch
%%% 3) Value for a gap
%%%
%%% These sample data should be used for sequences alignment.
-module(bio_erlang_producer).
-export([produce/2]).

%% Returns numeric random between specified min and max limits.
%% 1st param -> Minimum limit.
%% 2nd param -> Maximum limit.
random(Min, Max) ->
    Dif = Max - Min,
    random:uniform(Dif) + Min.

%% Returns a random char between A and J (inclusive).
random_char() ->
    random(65, 75).

%% Returns a random string with 15 chars.
random_string() ->
    [random_char() || _ <- lists:seq(1, 15)].

%% Returns a random 3-ple representing a setup to be used in the alignment.
random_setup() ->
    {
        random(1, 5),
        random(-5, 0),
        random(-5, 0)
    }.

%% Returns a 3-ple with two strings to be aligned and setup to the alignment.
random_sample() ->
    {
        random_string(),
        random_string(), 
        random_setup()
    }.

%% Produces some sample data to be aligned and sends to a handler process.
%% 1st param -> Number of samples to produce.
%% 2nd param -> Handler process' ID.
produce(0, HandlerPID) ->
    HandlerPID ! {producer, bye};
produce(Repetitions, HandlerPID) ->
    HandlerPID ! {producer, random_sample()},
    produce(Repetitions-1, HandlerPID).