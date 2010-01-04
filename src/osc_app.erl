%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc OSC Application.

-module(osc_app).
-author('ruslan@babayev.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @doc Starts the application.
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
start(_StartType, _StartArgs) ->
    osc_sup:start_link().

%% @doc Stops the application.
%% @spec stop(State) -> ok
stop(_State) ->
    ok.
