-module(sr_elements_SUITE).

-include_lib("mixer/include/mixer.hrl").

-mixin([{ sr_test_utils
        , [ init_per_suite/1
          , end_per_suite/1
          ]
        }]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).
-export([ success_scenario/1
        , duplicated_key/1
        , invalid_headers/1
        , invalid_parameters/1
        ]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
init_per_testcase(_, Config) ->
  _ = sumo:delete_all(sr_elements),
  Config.

-spec end_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
end_per_testcase(_, Config) ->
  Config.

-spec success_scenario(sr_test_utils:config()) -> {comment, string()}.
success_scenario(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},

  ct:comment("There are no elements"),
  #{status_code := 200, body := Body0} =
    sr_test_utils:api_call(get, "/elements"),
  [] = sr_json:decode(Body0),

  ct:comment("Element 1 is created"),
  #{status_code := 201, body := Body1} =
    sr_test_utils:api_call(
      post, "/elements", Headers,
      #{ key   => <<"element1">>
       , value => <<"val1">>
       }),
  #{ <<"key">>        := <<"element1">>
   , <<"created_at">> := CreatedAt
   , <<"updated_at">> := CreatedAt
   } = Element1 = sr_json:decode(Body1),

  ct:comment("There is one element now"),
  #{status_code := 200, body := Body2} =
    sr_test_utils:api_call(get, "/elements"),
  [Element1] = sr_json:decode(Body2),

  {comment, ""}.

-spec duplicated_key(sr_test_utils:config()) -> {comment, string()}.
duplicated_key(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  Body =
    #{ key   => <<"element1">>
     , value => <<"val1">>
     },

  ct:comment("Element 1 is created"),
  #{status_code := 201} =
    sr_test_utils:api_call(post, "/elements", Headers, Body),

  ct:comment("Element 1 can't be created again"),
  #{status_code := 409} =
    sr_test_utils:api_call(post, "/elements", Headers, Body),

  {comment, ""}.

-spec invalid_headers(sr_test_utils:config()) -> {comment, string()}.
invalid_headers(_Config) ->
  NoHeaders = #{},
  InvalidHeaders = #{<<"content-type">> => <<"text/plain">>},
  InvalidAccept = #{ <<"content-type">> => <<"application/json">>
                   , <<"accept">> => <<"text/html">>
                   },

  ct:comment("content-type must be provided for POST"),
  #{status_code := 415} =
    sr_test_utils:api_call(post, "/elements", NoHeaders, <<>>),

  ct:comment("content-type must be JSON for POST"),
  #{status_code := 415} =
    sr_test_utils:api_call(post, "/elements", InvalidHeaders, <<>>),

  ct:comment("Agent must accept json for POST, GET"),
  #{status_code := 406} =
    sr_test_utils:api_call(post, "/elements", InvalidAccept, <<>>),
  #{status_code := 406} =
    sr_test_utils:api_call(get, "/elements", InvalidAccept, <<>>),

  {comment, ""}.

-spec invalid_parameters(sr_test_utils:config()) -> {comment, string()}.
invalid_parameters(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},

  ct:comment("Empty or broken parameters are reported"),
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/elements", Headers, <<>>),
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/elements", Headers, <<"{">>),

  ct:comment("Missing parameters are reported"),
  None = #{},
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/elements", Headers, None),

  NoVal = #{key => <<"noval">>},
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/elements", Headers, NoVal),

  {comment, ""}.
