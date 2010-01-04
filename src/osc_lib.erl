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
    {bundle, decode_time(Time), decode_bundle(Rest, [])};
decode(<<"/", _/binary>> = Bin) ->
    {Address, Rest1} = decode_string(Bin),
    {Arguments, _} =
	try decode_string(Rest1) of
	    {[$,|Types], Rest2} ->
		decode_arguments(Rest2, Types);
	    _ ->
		{Rest1, <<>>}
	catch
	    _:_ ->
		{Rest1, <<>>}
	end,
    {message, Address, Arguments}.

%% @private Decodes bundles.
%% @spec decode_bundle(binary(), list()) -> [bundle()]
decode_bundle(<<>>, Acc) ->
    lists:reverse(Acc);
decode_bundle(<<Size:32, Bin:Size/binary, Rest/binary>>, Acc) ->
    decode_bundle(Rest, [decode(Bin)|Acc]).

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
    L = pad_len(size(B), 4),
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
	
decode_arguments(Bin, Types) ->
    decode_arguments(Bin, Types, []).

decode_arguments(Rest, [], Acc) ->
    {lists:reverse(Acc), Rest};
decode_arguments(<<Int32:32, Rest/binary>>, [$i|T], Acc) ->
    decode_arguments(Rest, T, [Int32|Acc]);
decode_arguments(<<Float:32/float, Rest/binary>>, [$f|T], Acc) ->
    decode_arguments(Rest, T, [Float|Acc]);
decode_arguments(Bin, [$s|T], Acc) ->
    {String, Rest} = decode_string(Bin),
    decode_arguments(Rest, T, [String|Acc]);
decode_arguments(Bin, [$b|T], Acc) ->
    {Blob, Rest} = decode_blob(Bin),
    decode_arguments(Rest, T, [Blob|Acc]);
decode_arguments(<<Int64:64, Rest/binary>>, [$h|T], Acc) ->
    decode_arguments(Rest, T, [Int64|Acc]);
decode_arguments(<<Time:8/binary, Rest/binary>>, [$t|T], Acc) ->
    decode_arguments(Rest, T, [decode_time(Time)|Acc]);
decode_arguments(<<Double:64/float, Rest/binary>>, [$d|T], Acc) ->
    decode_arguments(Rest, T, [Double|Acc]);
decode_arguments(Bin, [$S|T], Acc) ->
    {Symbol, Rest} = decode_string(Bin),
    decode_arguments(Rest, T, [list_to_atom(Symbol)|Acc]);
decode_arguments(<<Char:32, Rest/binary>>, [$c|T], Acc) ->
    decode_arguments(Rest, T, [Char|Acc]);
decode_arguments(<<R, G, B, A, Rest/binary>>, [$r|T], Acc) ->
    decode_arguments(Rest, T, [{R,G,B,A}|Acc]);
decode_arguments(<<Port, Status, Data1, Data2, Rest/binary>>, [$m|T], Acc) ->
    decode_arguments(Rest, T, [{Port,Status,Data1,Data2}|Acc]);
decode_arguments(Bin, [$T|T], Acc) ->
    decode_arguments(Bin, T, [true|Acc]);
decode_arguments(Bin, [$F|T], Acc) ->
    decode_arguments(Bin, T, [false|Acc]);
decode_arguments(Bin, [$N|T], Acc) ->
    decode_arguments(Bin, T, [null|Acc]);
decode_arguments(Bin, [$I|T], Acc) ->
    decode_arguments(Bin, T, [impulse|Acc]);
decode_arguments(Bin, [$[|T], Acc) ->
    {Array, RestBin, RestTypes} = decode_arguments(Bin, T, []),
    decode_arguments(RestBin, RestTypes, [Array|Acc]);
decode_arguments(Bin, [$]|T], Acc) ->
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
