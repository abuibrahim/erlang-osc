%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc OSC Supervisor

-module(osc_sup).
-author('ruslan@babayev.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes the supervisor.
%% @spec init(Args) -> {ok, {SupFlags, ChildSpecs}} | ignore | {error, Reason}
init([]) ->
    Server = {osc_server, {osc_server, start_link, []},
	      permanent, 2000, worker, [osc_server]},
    {ok, {{one_for_one, 3, 10}, [Server]}}.
