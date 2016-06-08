-module(producer).
-export([produce/2]).

random(Min, Max) ->
    Dif = Max - Min,
    random:uniform(Dif) + Min.

random_char() ->
    random(65, 75). % A to J

random_string() ->
    [random_char() || _ <- lists:seq(1, 15)].

random_setup() ->
    {
        random(1, 5),
        random(-5, 0),
        random(-5, 0)
    }.

random_sample() ->
    {
        random_string(),
        random_string(), 
        random_setup()
    }.

produce(0, TaskHandlerPID) ->
    TaskHandlerPID ! {producer, bye};
produce(Repetitions, TaskHandlerPID) ->
    TaskHandlerPID ! {producer, random_sample()},
    produce(Repetitions-1, TaskHandlerPID).