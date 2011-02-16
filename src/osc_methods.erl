%% @author Tobias Rodaebel
%% @doc Adding OSC methods.

-module(osc_methods).

-export([add_methods/0, delete_methods/0, log_data/1]).

-define(SERVER, {global, osc_server}).

%% @doc Adds methods.
%% @spec add_methods() -> ok
add_methods() ->
    gen_server:cast(?SERVER, {add_method, "/1/stop", ?MODULE, log_data}).

%% @doc Deletes methods.
%% @spec delete_methods() -> ok
delete_methods() ->
    gen_server:cast({global, osc_server}, {delete_method, "/1/stop"}).

%% @doc Logs handled data.
%% @spec log_data(Data) -> Data
log_data(Data) ->
    error_logger:info_msg("Received ~p", [Data]),
    Data.
