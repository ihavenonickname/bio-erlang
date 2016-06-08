-module(program).
-compile(producer).
-compile(consumer).
-export([start/2]).
-define(MAX_CONSUMER, 10).

start(NoProducers, NoItemsPerProducer) ->
    TaskHandlerPID = spawn(fun() -> task_handler(NoProducers, 0, []) end),
    spawn_producer(NoProducers, NoItemsPerProducer, TaskHandlerPID).

spawn_producer(NProducer, Repetitions, TaskHandlerPID) ->
    case NProducer of
        0 ->
            ok;
        _ ->
            spawn(fun() -> 
                random:seed(),
                producer:produce(Repetitions, TaskHandlerPID)
            end),
            spawn_producer(NProducer - 1, Repetitions, TaskHandlerPID)
    end.

should_continue(NProducer, NConsumer, Items) ->
    case {NProducer, NConsumer, Items} of
        {0, 0, []} -> no;
        _ -> yes
    end.

task_handler(NProducer, NConsumer, Items) ->
    receive
        {producer, bye} ->
            case should_continue(NProducer, NConsumer, Items) of
                no ->
                    ok;
                _ ->
                    task_handler(NProducer - 1, NConsumer, Items)
            end;
            
        {producer, Data} ->
            case NConsumer of
                ?MAX_CONSUMER ->
                    task_handler(NProducer, NConsumer, lists:append(Items, [Data]));
                _ ->
                    spawn(fun() -> consumer:consume(self(), Data) end),
                    task_handler(NProducer, NConsumer, Items)
            end;
            
        {consumer, ok} ->
            case should_continue(NProducer, NConsumer, Items) of
                no ->
                    ok;
                _ ->
                    case Items of
                        [] ->
                            task_handler(NProducer, NConsumer - 1, Items);
                        [H|T] ->
                            spawn(fun() -> consumer:consume(self(), H) end),
                            task_handler(NProducer, NConsumer, T)
                    end
            end
    end.