%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc Open Sound Control Server.
%%% @end
%%% Created : 15 Aug 2009 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(osc_server).
-author("ruslan@babayev.com").
-behaviour(gen_server).
-vsn("1.0.0").

%% API
-export([start_link/0, add_method/3, delete_method/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {socket, methods = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Adds a method.
%% @spec add_method(string(), atom(), atom()) -> none()
%% @end
%%--------------------------------------------------------------------
add_method(Address, Module, Function) ->
    gen_server:cast(?SERVER, {add_method, Address, Module, Function}).

%%--------------------------------------------------------------------
%% @doc Delete a method.
%% @spec delete_method(Address) -> none()
%% @end
%%--------------------------------------------------------------------
delete_method(Address) ->
    gen_server:cast(?SERVER, {delete_method, Address}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initiates the server.
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, RecBuf} = application:get_env(recbuf),
    Options = [binary, {active, once}, {recbuf, RecBuf}],
    case gen_udp:open(Port, Options) of
	{ok, Socket} ->
	    {ok, #state{socket = Socket}};
	{error, Reason} ->
	    error_logger:error_report({?MODULE,udp_open,Reason}),
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_method, Address, Module, Function}, State) ->
    Methods = State#state.methods,
    {noreply, State#state{methods = [{Address, Module, Function} | Methods]}};
handle_cast({delete_method, Address}, #state{methods = Methods} = State) ->
    {noreply, State#state{methods = lists:keydelete(Address, 1, Methods)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({udp, Socket, _IP, _Port, Packet}, State) ->
    inet:setopts(Socket, [{active, once}]),
    Methods = State#state.methods,
    try osc_lib:decode(Packet) of
	{message, Address, Args} ->
	    handle_message(immediately, Address, Args, Methods);
	{bundle, When, Elements} ->
	    handle_bundle(When, Elements, Methods)
    catch
	Class:Term ->
	    error_logger:error_report({osc_lib,decode,Class,Term})
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_message(When, Address, Args, Methods) ->
    case lists:keysearch(Address, 1, Methods) of
	{value, {Address, Module, Function}} ->
	    Time = when_to_msec(When),
	    timer:apply_after(Time, Module, Function, Args);
	false ->
	    error_logger:info_report({unhandled,{message,Address,Args}})
    end.

when_to_msec(immediately) ->
    0;
when_to_msec({Seconds, Fractions}) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    S = (Seconds - 2208988800) - (MegaSecs * 1000000 + Secs),
    F = Fractions - (MicroSecs * 1000000),
    case (S * 1000) + (1000 div F) of
	Time when Time > 0 ->
	    Time;
	_ ->
	    0
    end.

handle_bundle(_When, [], _Methods) ->
    ok;
handle_bundle(When, [{message, Address, Args} | Rest], Methods) ->
    handle_message(When, Address, Args, Methods),
    handle_bundle(When, Rest, Methods);
handle_bundle(When, [{bundle, InnerWhen, Elements} | Rest], Methods) ->
    handle_bundle(InnerWhen, Elements, Methods),
    handle_bundle(When, Rest, Methods).
