-module(sr_entities_SUITE).

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
        , not_found/1
        ]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
init_per_testcase(_, Config) ->
  _ = sumo:delete_all(sr_entities),
  Config.

-spec end_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
end_per_testcase(_, Config) ->
  Config.

-spec success_scenario(sr_test_utils:config()) -> {comment, string()}.
success_scenario(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},

  ct:comment("There are no entities"),
  #{status_code := 200, body := Body0} =
    sr_test_utils:api_call(get, "/entities"),
  [] = sr_json:decode(Body0),

  ct:comment("Entity 1 is created"),
  #{status_code := 201, body := Body1} =
    sr_test_utils:api_call(
      post, "/entities", Headers,
      #{ key   => <<"entity1">>
       , value => <<"val1">>
       }),
  #{ <<"key">>        := <<"entity1">>
   , <<"created_at">> := CreatedAt
   , <<"updated_at">> := CreatedAt
   } = Entity1 = sr_json:decode(Body1),

  ct:comment("There is one entity now"),
  #{status_code := 200, body := Body2} =
    sr_test_utils:api_call(get, "/entities"),
  [Entity1] = sr_json:decode(Body2),

  ct:comment("And we can fetch it"),
  #{status_code := 200, body := Body21} =
    sr_test_utils:api_call(get, "/entities/entity1"),
  Entity1 = sr_json:decode(Body21),

  ct:comment("The entity value can be changed"),
  #{status_code := 200, body := Body3} =
    sr_test_utils:api_call(
      put, "/entities/entity1", Headers,
      #{ value => <<"newval1">>
       }),
  #{ <<"key">>        := <<"entity1">>
   , <<"created_at">> := CreatedAt
   , <<"updated_at">> := UpdatedAt
   } = Entity3 = sr_json:decode(Body3),
  true = UpdatedAt > CreatedAt,

  ct:comment("Still just one entity"),
  #{status_code := 200, body := Body4} =
    sr_test_utils:api_call(get, "/entities"),
  [Entity3] = sr_json:decode(Body4),

  ct:comment("Entities can be created by PUT"),
  #{status_code := 201, body := Body5} =
    sr_test_utils:api_call(
      put, "/entities/entity2", Headers,
      #{value => <<"val2">>}),
  #{ <<"key">>        := <<"entity2">>
   , <<"created_at">> := CreatedAt5
   , <<"updated_at">> := CreatedAt5
   } = Entity5 = sr_json:decode(Body5),
  true = CreatedAt5 > CreatedAt,

  ct:comment("There are two entities now"),
  #{status_code := 200, body := Body6} =
    sr_test_utils:api_call(get, "/entities"),
  [Entity5] = sr_json:decode(Body6) -- [Entity3],

  ct:comment("Entity1 is deleted"),
  #{status_code := 204} = sr_test_utils:api_call(delete, "/entities/entity1"),

  ct:comment("One entity again"),
  #{status_code := 200, body := Body7} =
    sr_test_utils:api_call(get, "/entities"),
  [Entity5] = sr_json:decode(Body7),

  ct:comment("DELETE is not idempotent"),
  #{status_code := 204} = sr_test_utils:api_call(delete, "/entities/entity2"),
  #{status_code := 404} = sr_test_utils:api_call(delete, "/entities/entity2"),

  ct:comment("There are no entities"),
  #{status_code := 200, body := Body8} =
    sr_test_utils:api_call(get, "/entities"),
  [] = sr_json:decode(Body8),

  {comment, ""}.

-spec duplicated_key(sr_test_utils:config()) -> {comment, string()}.
duplicated_key(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  Body =
    #{ key   => <<"entity1">>
     , value => <<"val1">>
     },

  ct:comment("Entity 1 is created"),
  #{status_code := 201} =
    sr_test_utils:api_call(post, "/entities", Headers, Body),

  ct:comment("Entity 1 can't be created again"),
  #{status_code := 409} =
    sr_test_utils:api_call(post, "/entities", Headers, Body),

  {comment, ""}.

-spec invalid_headers(sr_test_utils:config()) -> {comment, string()}.
invalid_headers(_Config) ->
  NoHeaders = #{},
  InvalidHeaders = #{<<"content-type">> => <<"text/plain">>},
  InvalidAccept = #{ <<"content-type">> => <<"application/json">>
                   , <<"accept">> => <<"text/html">>
                   },

  ct:comment("content-type must be provided for POST and PUT"),
  #{status_code := 415} =
    sr_test_utils:api_call(post, "/entities", NoHeaders, <<>>),
  #{status_code := 415} =
    sr_test_utils:api_call(put, "/entities/noheaders", NoHeaders, <<>>),

  ct:comment("content-type must be JSON for POST and PUT"),
  #{status_code := 415} =
    sr_test_utils:api_call(post, "/entities", InvalidHeaders, <<>>),
  #{status_code := 415} =
    sr_test_utils:api_call(put, "/entities/badtype", InvalidHeaders, <<>>),

  ct:comment("Agent must accept json for POST, GET and PUT"),
  #{status_code := 406} =
    sr_test_utils:api_call(post, "/entities", InvalidAccept, <<>>),
  #{status_code := 406} =
    sr_test_utils:api_call(get, "/entities", InvalidAccept, <<>>),
  #{status_code := 406} =
    sr_test_utils:api_call(put, "/entities/badaccept", InvalidAccept, <<>>),
  #{status_code := 406} =
    sr_test_utils:api_call(get, "/entities/badaccept", InvalidAccept, <<>>),

  {comment, ""}.

-spec invalid_parameters(sr_test_utils:config()) -> {comment, string()}.
invalid_parameters(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},

  ct:comment("Empty or broken parameters are reported"),
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/entities", Headers, <<>>),
  #{status_code := 400} =
    sr_test_utils:api_call(put, "/entities/nobody", Headers, <<>>),
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/entities", Headers, <<"{">>),
  #{status_code := 400} =
    sr_test_utils:api_call(put, "/entities/broken", Headers, <<"{">>),

  ct:comment("Missing parameters are reported"),
  None = #{},
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/entities", Headers, None),
  #{status_code := 400} =
    sr_test_utils:api_call(put, "/entities/none", Headers, None),

  NoVal = #{key => <<"noval">>},
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/entities", Headers, NoVal),
  #{status_code := 400} =
    sr_test_utils:api_call(put, "/entities/noval", Headers, NoVal),

  {comment, ""}.

-spec not_found(sr_test_utils:config()) -> {comment, string()}.
not_found(_Config) ->
  ct:comment("Not existing entity is not found"),
  #{status_code := 404} = sr_test_utils:api_call(get, "/entities/notfound"),
  #{status_code := 404} = sr_test_utils:api_call(delete, "/entities/notfound"),
  {comment, ""}.
