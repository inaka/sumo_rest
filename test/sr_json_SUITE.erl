-module(sr_json_SUITE).

-export([all/0]).
-export([ jsons/1
        , dates/1
        , nulls/1
        , types/1
        ]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).

-spec jsons(sr_test_utils:config()) -> {comment, string()}.
jsons(_Config) ->
  WorksWith =
    fun(Json) ->
      ct:comment("Works with ~s", [Json]),
      Json = sr_json:encode(sr_json:decode(Json))
    end,
  Jsons =
    [ <<"null">>
    , <<"true">>
    , <<"false">>
    , <<"1">>
    , <<"0.3">>
    , <<"\"a string\"">>
    , <<"[true,true,false]">>
    , <<"{\"c\":[1,2,3],\"a\":\"b\"}">>
    ],
  lists:foreach(WorksWith, Jsons),

  ct:comment("Properly fails to decode {"),
  try sr_json:decode(<<"{">>) of
    R -> ct:fail("Unexpected result: ~p", [R])
  catch
    throw:badjson -> ok
  end,

  {comment, ""}.

-spec dates(sr_test_utils:config()) -> {comment, string()}.
dates(_Config) ->
  Now = calendar:universal_time(),
  ct:comment("Encodes ~p", [Now]),

  Now = sr_json:decode_date(sr_json:encode_date(Now)),

  {comment, ""}.

-spec nulls(sr_test_utils:config()) -> {comment, string()}.
nulls(_Config) ->
  ct:comment("Encodes undefined as null"),
  null = sr_json:encode_null(undefined),

  ct:comment("Leaves the rest untouched"),
  #{} = sr_json:encode_null(#{}),

  ct:comment("Decodes null as undefined"),
  undefined = sr_json:decode_null(null),

  ct:comment("Leaves the rest untouched"),
  #{} = sr_json:decode_null(#{}),

  {comment, ""}.

-spec types(sr_test_utils:config()) -> {comment, string()}.
types(_Config) ->
  JsonTypes = [ [<<"binary">>, <<"binary">>, <<"binary">>]
              , [1, 2, 3, 4, 5]
              , [#{a => 1}, #{b => 2}]
              , [<<"mixed">>, <<"list">>, 3, #{a => []}]
              ],
  _ = [sr_json:encode(Json) || Json <- JsonTypes],

  {comment, ""}.
