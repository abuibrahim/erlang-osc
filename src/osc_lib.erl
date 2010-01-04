%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc OSC Decoding Library.

-module(osc_lib).
-author("ruslan@babayev.com").

-export([decode/1]).

-include_lib("eunit/include/eunit.hrl").

%% @type message() = {message, Address::string(), args()}
%% @type args() = [integer() | float() | binary() | time() | atom() | time() |
%%                 rgba() | midi() | true | false | null | impulse | args()]
%% @type time() = immediately | {Seconds::integer(), Fractions::integer()}
%% @type rgba() = {R::integer(), G::integer(), B::integer(), A::integer()}
%% @type midi() = {Port::integer(), Status::integer(), binary(), binary()}
%% @type bundle() = {bundle, When::time(), [message() | bundle()]}

%% @doc Decodes messages.
%% @spec decode(Bytes::binary()) -> message() | bundle()
decode(<<"#bundle", 0, Time:8/binary, Rest/binary>>) ->
    {bundle, decode_time(Time), decode_bundle_elems(Rest, [])};
decode(<<"/", _/binary>> = Bin) ->
    {Address, Rest1} = decode_string(Bin),
    {Arguments, _} =
	try decode_string(Rest1) of
	    {[$,|Types], Rest2} ->
		decode_args(Rest2, Types);
	    _ ->
		{[Rest1], <<>>}
	catch
	    _:_ ->
		{[Rest1], <<>>}
	end,
    {message, Address, Arguments}.

%% @private
%% @doc Decodes bundle elements.
%% @spec decode_bundle_elems(Bytes::binary(), list()) -> [message() | bundle()]
decode_bundle_elems(<<>>, Acc) ->
    lists:reverse(Acc);
decode_bundle_elems(<<Size:32, Bin:Size/binary, Rest/binary>>, Acc) ->
    decode_bundle_elems(Rest, [decode(Bin)|Acc]).

%% @private
%% @doc Decodes times.
%% @spec decode_time(Bytes::binary()) -> time()
decode_time(<<1:64>>) ->
    immediately;
decode_time(<<Seconds:32, Fractions:32>>) ->
    {Seconds, Fractions}.

%% @private
%% @doc Decodes a padded and zero-terminated string.
%% @spec decode_string(Bytes::binary()) -> {String::string(), Rest::binary()}
decode_string(Bin) ->
    decode_string(Bin, []).

%% @private
%% @doc Decodes a padded and zero-terminated string.
%% @spec decode_string(Bytes::binary(), string()) -> {string(), binary()}
decode_string(<<0, Rest/binary>>, Acc) ->
    L = pad_len(length(Acc) + 1, 4),
    <<_:L/integer-unit:8, Rest1/binary>> = Rest,
    {lists:reverse(Acc), Rest1};
decode_string(<<Byte, Rest/binary>>, Acc) ->
    decode_string(Rest, [Byte|Acc]).

%% @hidden
decode_strings_test_() ->
    [?_assertEqual({"hello", <<>>}, decode_string(<<"hello",0,0,0>>)),
     ?_assertEqual({"hello1", <<>>}, decode_string(<<"hello1",0,0>>)),
     ?_assertEqual({"hello12", <<>>}, decode_string(<<"hello12",0>>)),
     ?_assertEqual({"hello123", <<>>}, decode_string(<<"hello123",0,0,0,0>>))].

%% @private
%% @doc Zero-pads the binary.
%% @spec pad(Bytes::binary(), Pad::integer()) -> binary()
pad(B, P) when is_binary(B), is_integer(P) ->
    L = pad_len(size(B), P),
    <<B/binary, 0:L/integer-unit:8>>.

%% @private
%% @doc Returns the length the binary has to be padded by.
%% @spec pad_len(Length::binary(), Padding::integer()) -> integer()
pad_len(L, P) when L rem P == 0 ->
    0;
pad_len(L, P) ->
    P - (L rem P).

%% @private
%% @doc Decodes a BLOB.
%% @spec decode_blob(Bytes::binary()) -> {Blob::binary(), Rest::binary()}
decode_blob(<<Length:32, Bytes:Length/binary, Rest/binary>>) ->
    L = pad_len(Length + 4, 4),
    <<_:L/integer-unit:8, Rest1/binary>> = Rest,
    {Bytes, Rest1}.

%% @hidden
decode_blobs_test_() ->
    [?_assertEqual({<<>>, <<>>}, decode_blob(<<0,0,0,0>>)),
     ?_assertEqual({<<1>>, <<>>}, decode_blob(<<0,0,0,1,1,0,0,0>>)),
     ?_assertEqual({<<1,2>>, <<>>}, decode_blob(<<0,0,0,2,1,2,0,0>>)),
     ?_assertEqual({<<1,2,3>>, <<>>}, decode_blob(<<0,0,0,3,1,2,3,0>>)),
     ?_assertEqual({<<1,2,3,4>>, <<>>}, decode_blob(<<0,0,0,4,1,2,3,4>>))].

%% @private
%% @doc Decodes arguments.
%% @spec decode_args(Bytes::binary(), Types::string()) -> args()
decode_args(Bin, Types) ->
    decode_args(Bin, Types, []).

%% @private
%% @doc Decodes arguments.
%% @spec decode_args(Bytes::binary(), Types::string(), Acc::args()) -> args()
decode_args(Rest, [], Acc) ->
    {lists:reverse(Acc), Rest};
decode_args(<<Int32:32, Rest/binary>>, [$i|T], Acc) ->
    decode_args(Rest, T, [Int32|Acc]);
decode_args(<<Float:32/float, Rest/binary>>, [$f|T], Acc) ->
    decode_args(Rest, T, [Float|Acc]);
decode_args(Bin, [$s|T], Acc) ->
    {String, Rest} = decode_string(Bin),
    decode_args(Rest, T, [String|Acc]);
decode_args(Bin, [$b|T], Acc) ->
    {Blob, Rest} = decode_blob(Bin),
    decode_args(Rest, T, [Blob|Acc]);
decode_args(<<Int64:64, Rest/binary>>, [$h|T], Acc) ->
    decode_args(Rest, T, [Int64|Acc]);
decode_args(<<Time:8/binary, Rest/binary>>, [$t|T], Acc) ->
    decode_args(Rest, T, [decode_time(Time)|Acc]);
decode_args(<<Double:64/float, Rest/binary>>, [$d|T], Acc) ->
    decode_args(Rest, T, [Double|Acc]);
decode_args(Bin, [$S|T], Acc) ->
    {Symbol, Rest} = decode_string(Bin),
    decode_args(Rest, T, [list_to_atom(Symbol)|Acc]);
decode_args(<<Char:32, Rest/binary>>, [$c|T], Acc) ->
    decode_args(Rest, T, [Char|Acc]);
decode_args(<<R, G, B, A, Rest/binary>>, [$r|T], Acc) ->
    decode_args(Rest, T, [{R,G,B,A}|Acc]);
decode_args(<<Port, Status, Data1, Data2, Rest/binary>>, [$m|T], Acc) ->
    decode_args(Rest, T, [{Port,Status,Data1,Data2}|Acc]);
decode_args(Bin, [$T|T], Acc) ->
    decode_args(Bin, T, [true|Acc]);
decode_args(Bin, [$F|T], Acc) ->
    decode_args(Bin, T, [false|Acc]);
decode_args(Bin, [$N|T], Acc) ->
    decode_args(Bin, T, [null|Acc]);
decode_args(Bin, [$I|T], Acc) ->
    decode_args(Bin, T, [impulse|Acc]);
decode_args(Bin, [$[|T], Acc) ->
    {Array, RestBin, RestTypes} = decode_args(Bin, T, []),
    decode_args(RestBin, RestTypes, [Array|Acc]);
decode_args(Bin, [$]|T], Acc) ->
    {lists:reverse(Acc), Bin, T}.

decode_arguments_test() ->
    Bin = <<1:32,100,97,116,97,0,0,0,0,1:32,4:32,5:32,2:32,3:32>>,
    Types = "is[i[ii]i]i",
    Args = [1,"data",[1,[4,5],2],3],
    ?assertEqual({Args, <<>>}, decode_arguments(Bin, Types, [])).

encode_string(S) when is_list(S) ->
    pad(list_to_binary(S ++ [0]), 4).

encode_strings_test_() ->
    [?_assertEqual(<<"hello",0,0,0>>, encode_string("hello")),
     ?_assertEqual(<<"hello1",0,0>>, encode_string("hello1")),
     ?_assertEqual(<<"hello12",0>>, encode_string("hello12")),
     ?_assertEqual(<<"hello123",0,0,0,0>>, encode_string("hello123"))].

encode_blob(B) when is_binary(B) ->
    pad(<<(size(B)):32, B/binary>>, 4).
%% @private
%% @doc Encodes the BLOB.
%% @spec encode_blob(binary()) -> Blob::binary()
encode_blob(Bin) when is_binary(Bin) ->
    pad(<<(byte_size(Bin)):32, Bin/binary>>, 4).

%% @hidden
encode_blobs_test_() ->
    [?_assertEqual(<<0,0,0,0>>, encode_blob(<<>>)),
     ?_assertEqual(<<0,0,0,1,1,0,0,0>>, encode_blob(<<1>>)),
     ?_assertEqual(<<0,0,0,2,1,2,0,0>>, encode_blob(<<1,2>>)),
     ?_assertEqual(<<0,0,0,3,1,2,3,0>>, encode_blob(<<1,2,3>>)),
     ?_assertEqual(<<0,0,0,4,1,2,3,4>>, encode_blob(<<1,2,3,4>>))].
