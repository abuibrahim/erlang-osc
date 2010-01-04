%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev

-module(osc).
-author('ruslan@babayev.com').

-export([start/0, stop/0]).

%% @doc Starts the application.
%% @spec start() -> ok | {error, Reason}
start() ->
    application:start(osc).

%% @doc Stops the application.
%% @spec stop() -> ok
stop() ->
    application:stop(osc).
