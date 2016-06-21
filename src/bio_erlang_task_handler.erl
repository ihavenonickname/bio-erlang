%%% The module to handle all data produced and consumed.
%%%
%%% This module manages the balance between producers and consumers, as well
%%% handle the data produced and sends it to be consumed.
-module(bio_erlang_task_handler).
-export([handle/2]).
-define(MAX_CONSUMER, 10).

%% Returns atom ok after spawns the specified number of producers.
%% 1st param -> Number of producers to spawn.
%% 2nd param -> Number of repetition for each producer.
%% 3rd param -> Handler process' ID.
spawn_producer(NoProducers, Repetitions, HandlePID) ->
    case NoProducers of
        0 ->
            ok;
        _ ->
            spawn(fun() -> 
                random:seed(),
                bio_erlang_producer:produce(Repetitions, HandlePID)
            end),
            spawn_producer(NoProducers - 1, Repetitions, HandlePID)
    end.

%% Returns yes|no indicating if handler should keep running.
%% 1st param -> Current number of producers. 
%% 2nd param -> Current number of consumers.
%% 3rd param -> List with items to be consumed.  
should_continue(NoProducers, NoConsumers, Items) ->
    case {NoProducers, NoConsumers, Items} of
        {0, 0, []} ->
            no;
        _ ->
            yes
    end.

%% Returns the new number of consumers and the items to be consumed.
%% 1st param -> Current number of consumers. 
%% 2nd param -> List with items to be consumed.
%% 3rd param -> Item just received.
%% 4th param -> Handler process' ID.
handle_data(NoConsumers, Items, Data, HandlerPID) ->
    case NoConsumers of
        ?MAX_CONSUMER ->
            {NoConsumers, lists:append(Items, [Data])};
        _ ->
            spawn(fun() -> bio_erlang_consumer:consume(HandlerPID, Data) end),
            {NoConsumers + 1, Items}
    end.

%% Returns the new number of consumers and the items to be consumed.
%% 1st param -> Current number of consumers. 
%% 2nd param -> List with items to be consumed.
%% 3rd param -> Handler process' ID.
handle_finished_consumer(NoConsumers, Items, HandlerPID) ->
    case Items of
        [] ->
            {NoConsumers - 1, Items};
        [H|T] ->
            spawn(fun() -> bio_erlang_consumer:consume(HandlerPID, H) end),
            {NoConsumers, T}
    end.

%% Returns atom ok after handle all producers and consumers.
%% 1st param -> Current number of producers.
%% 2nd param -> Current number of consumers.
%% 3rd param -> List with items to be consumed.
handle_helper(NoProducers, NoConsumers, Items) ->
    receive
        {producer, bye} ->
            case should_continue(NoProducers, NoConsumers, Items) of
                yes ->
                    handle_helper(NoProducers - 1, NoConsumers, Items);
                no ->
                    ok
            end;
            
        {producer, Data} ->
            {NewNoConsumers, NewItems} = handle_data(NoConsumers, Items, Data, self()),
            handle_helper(NoProducers, NewNoConsumers, NewItems);
            
        {consumer, ok} ->
            case should_continue(NoProducers, NoConsumers - 1, Items) of
                yes ->
                    {NewNoConsumers, NewItems} = handle_finished_consumer(NoConsumers, Items, self()),
                    handle_helper(NoProducers, NewNoConsumers, NewItems);
                no ->
                    ok
            end;

        BadMessage ->
            io:format("Bad message received: ~w ~n", [BadMessage])
    end.

%% Returns atom ok after handle all producers and consumers.
%% 1st param -> Number of producers.
%% 2nd param -> Number of repetition for each producer.
handle(NoProducers, NoItemsPerProducer) ->
    HandlerPID = spawn(fun() -> handle_helper(NoProducers, 0, []) end),
    spawn_producer(NoProducers, NoItemsPerProducer, HandlerPID).
