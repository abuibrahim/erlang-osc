%% @author Ruslan Babayev <ruslan@babayev.com>
%% @author Tobias Rodaebel <tobias.rodaebel@googlemail.com>
%% @copyright 2009 Ruslan Babayev
%% @doc OSC Decoding/Encoding Library.

-module(osc_lib).
-author("ruslan@babayev.com").
-author("tobias.rodaebel@googlemail.com").

-export([decode/1, encode/1]).

-include_lib("eunit/include/eunit.hrl").

%% @type message() = {message, Address::string(), args()}
%% @type args() = [integer() | float() | binary() | time() | atom() | time() |
%%                 rgba() | midi() | true | false | null | impulse | args()]
%% @type time() = immediately | {time, Seconds::integer(), Fractions::integer()}
%% @type rgba() = {rgba, R::integer(), G::integer(), B::integer(), A::integer()}
%% @type midi() = {midi, Port::integer(), Status::integer(), binary(), binary()}
%% @type bundle() = {bundle, When::time(), [message() | bundle()]}

%% @doc Decodes messages.
%% @spec decode(Bytes::binary()) -> message() | bundle()
decode(<<"#bundle", 0, Time:8/binary, Rest/binary>>) ->
    {bundle, decode_time(Time), decode_bundle_elems(Rest, [])};
decode(<<"/", _/binary>> = Bin) ->
    {Address, Rest1} = decode_string(Bin),
    {Args, _} =
	try decode_string(Rest1) of
	    {[$,|Types], Rest2} ->
		decode_args(Rest2, Types);
	    _ ->
		{[Rest1], <<>>}
	catch
	    _:_ ->
		{[Rest1], <<>>}
	end,
    {message, Address, Args}.

%% @doc Decodes bundle elements.
%% @spec decode_bundle_elems(Bytes::binary(), list()) -> [message() | bundle()]
decode_bundle_elems(<<>>, Acc) ->
    lists:reverse(Acc);
decode_bundle_elems(<<Size:32, Bin:Size/binary, Rest/binary>>, Acc) ->
    decode_bundle_elems(Rest, [decode(Bin)|Acc]).

%% @doc Decodes times.
%% @spec decode_time(Bytes::binary()) -> time()
decode_time(<<1:64>>) ->
    immediately;
decode_time(<<Seconds:32, Fractions:32>>) ->
    {time, Seconds, Fractions}.

%% @doc Decodes a padded and zero-terminated string.
%% @spec decode_string(Bytes::binary()) -> {String::string(), Rest::binary()}
decode_string(Bin) ->
    decode_string(Bin, []).

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

%% @doc Zero-pads the binary.
%% @spec pad(Bytes::binary(), Pad::integer()) -> binary()
pad(B, P) when is_binary(B), is_integer(P) ->
    L = pad_len(size(B), P),
    <<B/binary, 0:L/integer-unit:8>>.

%% @doc Returns the length the binary has to be padded by.
%% @spec pad_len(Length::binary(), Padding::integer()) -> integer()
pad_len(L, P) when L rem P == 0 ->
    0;
pad_len(L, P) ->
    P - (L rem P).

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

%% @doc Decodes arguments.
%% @spec decode_args(Bytes::binary(), Types::string()) -> args()
decode_args(Bin, Types) ->
    decode_args(Bin, Types, []).

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
    decode_args(Rest, T, [{rgba,R,G,B,A}|Acc]);
decode_args(<<Port, Status, Data1, Data2, Rest/binary>>, [$m|T], Acc) ->
    decode_args(Rest, T, [{midi,Port,Status,Data1,Data2}|Acc]);
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

%% @hidden
decode_args_test() ->
    Bin = <<1:32,102,111,111,0,1:32,4:32,5:32,2:32,3:32,255,255,255,255>>,
    Types = "is[i[ii]i]ir",
    Args = [1,"foo",[1,[4,5],2],3,{rgba, 255,255,255,255}],
    ?assertEqual({Args, <<>>}, decode_args(Bin, Types, [])).

%% @doc Encodes messages.
%% @spec encode(Data::message()|bundle()) -> Bytes::binary()
encode({bundle, Time, Elems}) ->
    Bin = [[<<"#bundle",0>>,encode_time(Time)], encode_bundle_elems(Elems, [])],
    list_to_binary(Bin);
encode({message, Address, Args}) ->
    Bytes = encode_string(Address),
    {Data, Types} = encode_args(Args),
    list_to_binary([<<Bytes/binary>>,[encode_types(Types, []),Data]]).

%% @hidden
encode_test_() ->
    Message1 = {message, "/1/play", [1.0]},
    Result1 = <<47,49,47,112,108,97,121,0,44,102,0,0,63,128,0,0>>,
    Message2 = {message,"/1/xy1",[1.0,1.0]},
    Result2 = <<47,49,47,120,121,49,0,0,44,102,102,0,63,128,0,0,63,128,0,0>>,
    Bundle = {bundle, {time, 10, 5},
              [{message, "/1/play", [1.0]}, {message, "/1/xy1", [1.0,1.0]}]},
    Result3 = <<35,98,117,110,100,108,101,0,0,0,0,10,0,0,0,5,0,0,0,16,47,49,
                47,112,108,97,121,0,44,102,0,0,63,128,0,0,0,0,0,20,47,49,47,
                120,121,49,0,0,44,102,102,0,63,128,0,0,63,128,0,0>>,
    [?_assertEqual(Result1, encode(Message1)),
     ?_assertEqual(Message1, decode(Result1)),
     ?_assertEqual(Result2, encode(Message2)),
     ?_assertEqual(Message2, decode(Result2)),
     ?_assertEqual(Result3, encode(Bundle)),
     ?_assertEqual(Bundle, decode(Result3))].

%% @doc Encodes bundle elements.
%% @spec encode_bundle_elems([message() | bundle()], list()) -> binary()
encode_bundle_elems([], Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_bundle_elems([Element|Rest], Acc) ->
    Bin = encode(Element),
    Size = size(Bin),
    encode_bundle_elems(Rest, [[<<Size:32,Bin:Size/binary>>]|Acc]).

%% @hidden
encode_bundle_elems_test_() ->
    Messages = [{message, "/1/play", [1.0]}, {message, "/1/xy1", [1.0,1.0]}],
    Result = <<0,0,0,16,47,49,47,112,108,97,121,0,44,102,0,0,63,128,0,0,
    0,0,0,20,47,49,47,120,121,49,0,0,44,102,102,0,63,128,0,0,63,128,0,0>>,
    [?_assertEqual(Result, encode_bundle_elems(Messages, [])),
     ?_assertEqual(Messages, decode_bundle_elems(Result, []))].

%% @doc Encodes times.
%% @spec encode_time(Time::time()) -> binary()
encode_time(immediately) ->
    <<1:64>>;
encode_time({time, Seconds, Fractions}) ->
    <<Seconds:32, Fractions:32>>.

%% @hidden
encode_time_test_() ->
    [?_assertEqual(<<1:64>>, encode_time(immediately)),
     ?_assertEqual(<<10:32,5:32>>, encode_time({time, 10, 5}))].

%% @doc Encodes the string by zero-terminating it and padding to 4 chars.
%% @spec encode_string(string()) -> binary()
encode_string(String) when is_list(String) ->
    pad(list_to_binary(String ++ [0]), 4).

%% @hidden
encode_strings_test_() ->
    [?_assertEqual(<<"hello",0,0,0>>, encode_string("hello")),
     ?_assertEqual(<<"hello1",0,0>>, encode_string("hello1")),
     ?_assertEqual(<<"hello12",0>>, encode_string("hello12")),
     ?_assertEqual(<<"hello123",0,0,0,0>>, encode_string("hello123"))].

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

%% @doc Checks whether a list is a string.
%% @spec is_string(List) -> true | false
is_string([]) -> true;
is_string([H|_]) when not is_integer(H) -> false;
is_string([_|T]) -> is_string(T);
is_string(_) -> false.

%% @hidden
is_string_test_() ->
    [?_assertEqual(true, is_string("foo")),
     ?_assertEqual(false, is_string([one,2]))].

%% @doc Encodes args.
%% @spec encode_args(Args::args()) -> Bytes::binary()
encode_args(Args) ->
    encode_args(Args, [], []).

encode_args([], Acc, Types) ->
    {list_to_binary(Acc), lists:flatten(Types)};
encode_args([{i,Int32}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<Int32:32>>], [Types,$i]);
encode_args([Int32|Rest], Acc, Types) when is_integer(Int32) ->
    encode_args([{i,Int32}|Rest], Acc, Types);
encode_args([{f,Float}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<Float:32/float>>], [Types,$f]);
encode_args([Float|Rest], Acc, Types) when is_float(Float) ->
    encode_args([{f,Float}|Rest], Acc, Types);
encode_args([{s,String}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,encode_string(String)], [Types,$s]);
encode_args([{b,Blob}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,encode_blob(Blob)], [Types,$b]);
encode_args([{h,Int64}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<Int64:64>>], [Types,$h]);
encode_args([{time,Seconds,Fractions}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,encode_time({time,Seconds,Fractions})], [Types,$t]);
encode_args([immediately|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,encode_time(immediately)], [Types,$t]);
encode_args([{d,Double}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<Double:64/float>>], [Types,$d]);
encode_args([true|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<>>], [Types,$T]);
encode_args([false|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<>>], [Types,$F]);
encode_args([null|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<>>], [Types,$N]);
encode_args([impulse|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<>>], [Types,$I]);
encode_args([Symbol|Rest], Acc, Types) when is_atom(Symbol) ->
    encode_args(Rest, [Acc,encode_string(atom_to_list(Symbol))], [Types,$S]);
encode_args([{c,Char}|Rest], Acc, Types) when is_integer(Char) ->
    encode_args(Rest, [Acc,<<Char:32>>], [Types,$c]);
encode_args([{rgba,R,G,B,A}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<R/integer,G/integer,B/integer,A/integer>>],
                [Types,$r]);
encode_args([{midi,Port,Status,Data1,Data2}|Rest], Acc, Types) ->
    encode_args(Rest, [Acc,<<Port/integer,Status/integer,Data1/binary,Data2/binary>>], [Types,$m]);
encode_args([L|Rest], Acc, Types) when is_list(L) ->
    case is_string(L) of
        true ->
            encode_args([{s,L}|Rest], Acc, Types);
        false ->
            {Bytes, Types2} = encode_args(L, [], []),
            encode_args(Rest, [Acc,Bytes], [Types,$[,Types2,$]])
    end.

%% @hidden
encode_args_test() ->
    Bin = <<1:32,2:32,1:64,100,97,116,97,0,0,0,0,2.5:32/float,42:64,
            0,0,0,1,1,0,0,0,102,111,111,0,97:32,255,255,255,255>>,
    Types = "i[it[sf]h]bScrTFI",
    A = [{i,1},[{i,2},immediately,[{s,"data"},{f,2.5}],{h,42}],{b,<<1>>},foo,
         {c,$a},{rgba,255,255,255,255},true,false,impulse],
    ?assertEqual({Bin, Types}, encode_args(A)),
    B = [1,[2,immediately,["data",2.5],{h,42}],{b,<<1>>},'foo',{c,$a},
         {rgba,255,255,255,255},true,false,impulse],
    ?assertEqual({Bin, Types}, encode_args(B)).

%% @doc Encodes type identifiers
%% @spec encode_types(Types, []) -> binary()
encode_types([], Acc) ->
    pad(list_to_binary([<<$,>>,Acc]), 4);
encode_types([Type|Rest], Acc) ->
    encode_types(Rest, [Acc,<<Type/integer>>]).

%% @hidden
encode_types_test() ->
    ?assertEqual(<<44,102,102,0>>, encode_types("ff", [])).
