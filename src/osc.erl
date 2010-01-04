%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev

-module(osc).
-author('ruslan@babayev.com').

-export([start/0, stop/0]).

start() ->
    application:start(osc).

stop() ->
    application:stop(osc).
