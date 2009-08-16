%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc Open Sound Control Library.
%%% @end
%%% Created : 15 Aug 2009 by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(osc_lib).
-author("ruslan@babayev.com").

-export([decode/1]).

-include_lib("eunit/include/eunit.hrl").
-include("osc.hrl").


%% @doc Returns a message or a bundle.
%% @spec decode(binary()) -> #osc_message{} | #osc_bundle{}
decode(Bin) when is_binary(Bin) ->
    decode(Bin, []).

decode(<<>>, Acc) ->
    lists:reverse(Acc);
decode(<<Size:32, Bin:Size/binary, Rest/binary>>, Acc) ->
    decode(Rest, [decode_packet(Bin)|Acc]).
    
decode_packet(<<"#bundle", 0, Time:8/binary, Rest/binary>>) ->
    #osc_bundle{time = decode_time(Time), elements = decode(Rest)};
decode_packet(<<"/", _/binary>> = Bin) ->
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
    #osc_message{address = Address, arguments = Arguments}.

decode_time(<<1:64>>) ->
    immediately;
decode_time(<<Seconds:32, Fractions:32>>) ->
    Seconds bsr Fractions.

decode_string(Bin) ->
    decode_string(Bin, []).

decode_string(<<0, Rest/binary>>, Acc) ->
    L = pad_len(length(Acc) + 1, 4),
    <<_:L/integer-unit:8, Rest1/binary>> = Rest,
    {lists:reverse(Acc), Rest1};
decode_string(<<Byte, Rest/binary>>, Acc) ->
    decode_string(Rest, [Byte|Acc]).

decode_string1_test() ->
    ?assertEqual({"hello", <<>>}, decode_string(<<"hello",0,0,0>>)).

decode_string2_test() ->
    ?assertEqual({"hello1", <<>>}, decode_string(<<"hello1",0,0>>)).

decode_string3_test() ->
    ?assertEqual({"hello12", <<>>}, decode_string(<<"hello12",0>>)).

decode_string4_test() ->
    ?assertEqual({"hello123", <<>>}, decode_string(<<"hello123",0,0,0,0>>)).

pad(B, P) when is_binary(B), is_integer(P) ->
    L = pad_len(size(B), 4),
    <<B/binary, 0:L/integer-unit:8>>.

pad_len(L, P) when L rem P == 0 ->
    0;
pad_len(L, P) ->
    P - (L rem P).

decode_blob(<<Length:32, Bytes:Length/binary, Rest/binary>>) ->
    L = pad_len(Length + 4, 4),
    <<_:L/integer-unit:8, Rest1/binary>> = Rest,
    {Bytes, Rest1}.

decode_blob0_test() ->
    ?assertEqual({<<>>, <<>>}, decode_blob(<<0,0,0,0>>)).

decode_blob1_test() ->
    ?assertEqual({<<1>>, <<>>}, decode_blob(<<0,0,0,1,1,0,0,0>>)).

decode_blob2_test() ->
    ?assertEqual({<<1,2>>, <<>>}, decode_blob(<<0,0,0,2,1,2,0,0>>)).

decode_blob3_test() ->
    ?assertEqual({<<1,2,3>>, <<>>}, decode_blob(<<0,0,0,3,1,2,3,0>>)).

decode_blob4_test() ->
    ?assertEqual({<<1,2,3,4>>, <<>>}, decode_blob(<<0,0,0,4,1,2,3,4>>)).

decode_arguments(Bin, Types) ->
    decode_arguments(Bin, Types, []).

decode_arguments(Rest, [], Acc) ->
    {lists:reverse(Acc), Rest};
decode_arguments(<<Arg:32, Rest/binary>>, [$i|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(<<Arg:32/float, Rest/binary>>, [$f|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(Bin, [$s|T], Acc) ->
    {Arg, Rest} = decode_string(Bin),
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(Bin, [$b|T], Acc) ->
    {Arg, Rest} = decode_blob(Bin),
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(<<Arg:64, Rest/binary>>, [$h|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(<<Arg:8/binary, Rest/binary>>, [$t|T], Acc) ->
    decode_arguments(Rest, T, [decode_time(Arg)|Acc]);
decode_arguments(<<Arg:64/float, Rest/binary>>, [$d|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(Bin, [$S|T], Acc) ->
    {Arg, Rest} = decode_string(Bin),
    decode_arguments(Rest, T, [list_to_atom(Arg)|Acc]);
decode_arguments(<<Arg:32, Rest/binary>>, [$c|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(<<Arg:32, Rest/binary>>, [$r|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(<<Arg:32, Rest/binary>>, [$m|T], Acc) ->
    decode_arguments(Rest, T, [Arg|Acc]);
decode_arguments(Bin, [$T|T], Acc) ->
    decode_arguments(Bin, T, [true|Acc]);
decode_arguments(Bin, [$F|T], Acc) ->
    decode_arguments(Bin, T, [false|Acc]);
decode_arguments(Bin, [$N|T], Acc) ->
    decode_arguments(Bin, T, [nil|Acc]);
decode_arguments(Bin, [$I|T], Acc) ->
    decode_arguments(Bin, T, [infinity|Acc]);
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

encode_string1_test() ->
    ?assertEqual(<<"hello",0,0,0>>, encode_string("hello")).

encode_string2_test() ->
    ?assertEqual(<<"hello1",0,0>>, encode_string("hello1")).

encode_string3_test() ->
    ?assertEqual(<<"hello12",0>>, encode_string("hello12")).

encode_string4_test() ->
    ?assertEqual(<<"hello123",0,0,0,0>>, encode_string("hello123")).

encode_blob(B) when is_binary(B) ->
    pad(<<(size(B)):32, B/binary>>, 4).

encode_blob0_test() ->
    ?assertEqual(<<0,0,0,0>>, encode_blob(<<>>)).

encode_blob1_test() ->
    ?assertEqual(<<0,0,0,1,1,0,0,0>>, encode_blob(<<1>>)).

encode_blob2_test() ->
    ?assertEqual(<<0,0,0,2,1,2,0,0>>, encode_blob(<<1,2>>)).

encode_blob3_test() ->
    ?assertEqual(<<0,0,0,3,1,2,3,0>>, encode_blob(<<1,2,3>>)).

encode_blob4_test() ->
    ?assertEqual(<<0,0,0,4,1,2,3,4>>, encode_blob(<<1,2,3,4>>)).
