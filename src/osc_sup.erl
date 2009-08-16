%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc Open Sound Control Supervisor
%%% @end
%%% Created : 15 Aug 2009 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(osc_sup).
-author('ruslan@babayev.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Server = {osc_server, {osc_server, start_link, []},
	      permanent, 2000, worker, [osc_server]},
    {ok, {{one_for_one, 3, 10}, [Server]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
