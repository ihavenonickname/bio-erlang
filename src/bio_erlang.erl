-module(bio_erlang).
-export([start/0]).

start() ->
    bio_erlang_task_handler:handle(5, 3).