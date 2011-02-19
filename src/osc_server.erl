%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc OSC Server.

-module(osc_server).
-author("ruslan@babayev.com").
-vsn("1.0.0").

-behaviour(gen_server).

%% API
-export([start_link/0, add_method/3, delete_method/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {socket, methods}).

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%% @doc Adds a method.
%% @spec add_method(string(), atom(), atom()) -> ok
add_method(Address, Module, Function) ->
    gen_server:cast(?SERVER, {add_method, Address, Module, Function}).

%% @doc Deletes a method.
%% @spec delete_method(Address) -> ok
delete_method(Address) ->
    gen_server:cast(?SERVER, {delete_method, Address}).

%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, RecBuf} = application:get_env(recbuf),
    Options = [binary, {active, once}, {recbuf, RecBuf}],
    case gen_udp:open(Port, Options) of
	{ok, Socket} ->
	    Methods = ets:new(osc_methods, [named_table, ordered_set]),
	    {ok, #state{socket = Socket, methods = Methods}};
	{error, Reason} ->
	    error_logger:error_report({?MODULE, udp_open, Reason}),
	    {stop, {?MODULE, udp_open, Reason}}
    end.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) ->
%%                  {reply, Reply, State} |
%%                  {reply, Reply, State, Timeout} |
%%                  {noreply, State} |
%%                  {noreply, State, Timeout} |
%%                  {stop, Reason, Reply, State} |
%%                  {stop, Reason, State}
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast({add_method, Address, Module, Function}, State) ->
    Methods = State#state.methods,
    ets:insert(Methods, {string:tokens(Address, "/"), {Module, Function}}),
    {noreply, State};
handle_cast({delete_method, Address}, State) ->
    Methods = State#state.methods,
    ets:delete(Methods, string:tokens(Address, "/")),
    {noreply, State}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
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

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> void()
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
    ok.

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Handles OSC messages.
%% @spec handle_message(When, Address, Args, Methods) -> any()
%%       When = time()
%%       Address = string()
%%       Args = [any()]
%%       Methods = [method()]
handle_message(When, Address, Args, Methods) ->
    case ets:match(Methods, make_pattern(Address)) of
	[] ->
	    error_logger:info_report({unhandled,{message,Address,Args}});
	Matches ->
	    Time = when_to_millisecs(When),
	    [timer:apply_after(Time, Module, Function, Args) ||
		[{Module, Function}] <- Matches]
    end.

%% @doc Converts the OSC address pattern into ETS match spec.
%% @spec make_pattern(string()) -> tuple()
make_pattern(Address) ->
    make_pattern(string:tokens(Address, "/"), []).

make_pattern([], Acc) ->
    {lists:reverse(Acc), '$1'};
make_pattern(["*"], Acc) ->
    {lists:reverse(Acc) ++ '_', '$1'};
make_pattern([H|T], Acc) ->
    make_pattern(T, [make_pattern2(H, [])|Acc]).

make_pattern2([], Acc) ->
    lists:reverse(Acc);
make_pattern2([$*|_], Acc) ->
    lists:reverse(Acc) ++ '_';
make_pattern2([$?|T], Acc) ->
    make_pattern2(T, ['_'|Acc]);
make_pattern2([H|T], Acc) ->
    make_pattern2(T, [H|Acc]).

%% @doc Converts OSC time to milliseconds.
%% @spec when_to_millisecs(When) -> integer()
%%       When = immediately | {time, Seconds::integer(), Fractions::integer()}
when_to_millisecs(immediately) ->
    0;
when_to_millisecs({time, Seconds, Fractions}) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    S = (Seconds - 2208988800) - (MegaSecs * 1000000 + Secs),
    F = Fractions - (MicroSecs * 1000000),
    case (S * 1000) + (1000 div F) of
	Time when Time > 0 ->
	    Time;
	_ ->
	    0
    end.

%% @doc Handles OSC bundles.
%% @spec handle_bundle(When, Elements, Methods) -> any()
%%       Elements = [message() | bundle()]
%%       Methods = [method()]
%%       message() = {message, Address::string(), Args::[any()]}
%%       bundle() = {bundle, When::time(), [message() | bundle()]}
%%       time() = immediately | {time, Seconds::integer(), Fractions::integer()}
%%       method() = {Module::atom(), Function::atom()}
handle_bundle(_When, [], _Methods) ->
    ok;
handle_bundle(When, [{message, Address, Args} | Rest], Methods) ->
    handle_message(When, Address, Args, Methods),
    handle_bundle(When, Rest, Methods);
handle_bundle(When, [{bundle, InnerWhen, Elements} | Rest], Methods) ->
    handle_bundle(InnerWhen, Elements, Methods),
    handle_bundle(When, Rest, Methods).
