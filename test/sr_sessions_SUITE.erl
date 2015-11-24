-module(sr_sessions_SUITE).

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
        , invalid_auth/1
        , invalid_headers/1
        ]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
init_per_testcase(_, Config) ->
  _ = sumo:delete_all(sr_sessions),
  [{U, P}|_] = application:get_env(sr_test, users, []),
  [{basic_auth, {binary_to_list(U), binary_to_list(P)}} | Config].

-spec end_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
end_per_testcase(_, Config) ->
  Config.

-spec success_scenario(sr_test_utils:config()) -> {comment, string()}.
success_scenario(Config) ->
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  Headers = #{ basic_auth => BasicAuth
             , <<"content-type">> => <<"application/json">>
             },

  ct:comment("There are no sessions"),
  [] = sumo:find_all(sr_sessions),

  ct:comment("A session is created"),
  #{status_code := 201, body := Body1} =
    sr_test_utils:api_call(post, "/sessions", Headers),
  #{ <<"id">>           := Session1Id
   , <<"token">>        := Token1
   , <<"created_at">>   := CreatedAt1
   , <<"expires_at">>   := ExpiresAt1
   } = sr_json:decode(Body1),
  true = ExpiresAt1 >= CreatedAt1,

  ct:comment("Session ~s is there", [Session1Id]),
  [Session1] = sumo:find_all(sr_sessions),
  Token1 = sr_sessions:token(Session1),

  ct:comment("Another session can be created with the same parameters"),
  #{status_code := 201, body := Body2} =
    sr_test_utils:api_call(post, "/sessions", Headers),
  #{ <<"id">>           := Session2Id
   , <<"token">>        := Token2
   , <<"created_at">>   := CreatedAt2
   , <<"expires_at">>   := ExpiresAt2
   } = sr_json:decode(Body2),
  true = ExpiresAt2 >= CreatedAt2,
  true = ExpiresAt2 >= ExpiresAt1,
  true = CreatedAt2 >= CreatedAt1,
  case Session2Id of
    Session1Id -> ct:fail("Duplicated Session Id: ~p", [Session1Id]);
    Session2Id -> ok
  end,
  case Token2 of
    Token1 -> ct:fail("Duplicated token: ~p", [Token1]);
    Token2 -> ok
  end,

  ct:comment("There are 2 sessions"),
  [Session2] = sumo:find_all(sr_sessions) -- [Session1],

  ct:comment("Session1 is deleted"),
  Uri1 = binary_to_list(<<"/sessions/", Session1Id/binary>>),
  #{status_code := 204} = sr_test_utils:api_call(delete, Uri1, Headers),

  ct:comment("One session again"),
  [Session2] = sumo:find_all(sr_sessions),

  ct:comment("DELETE is not idempotent"),
  Uri2 = binary_to_list(<<"/sessions/", Session2Id/binary>>),
  #{status_code := 204} = sr_test_utils:api_call(delete, Uri2, Headers),
  #{status_code := 404} = sr_test_utils:api_call(delete, Uri2, Headers),

  ct:comment("There are no sessions"),
  [] = sumo:find_all(sr_sessions),

  {comment, ""}.

-spec invalid_auth(sr_test_utils:config()) -> {comment, string()}.
invalid_auth(Config) ->
  Uri = "/sessions",

  ct:comment("Can't use POST nor DELETE without auth"),
  #{status_code := 401} = sr_test_utils:api_call(post, Uri),
  #{status_code := 401} = sr_test_utils:api_call(delete, Uri ++ "/noauth"),

  ct:comment("Can't use POST nor DELETE without Basic auth"),
  Headers1 = #{<<"Authorization">> => <<"Bearer iSnotAGoodThing">>},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers1),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/other", Headers1),

  ct:comment("Can't use POST nor DELETE with broken Basic auth"),
  Headers2 = #{<<"Authorization">> => <<"Basic ThisIsNotBase64">>},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers2),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/broken", Headers2),

  ct:comment("Can't use POST nor DELETE with wrong user"),
  Headers3 = #{basic_auth => {"not-user", "pwd"}},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers3),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/not-user", Headers3),

  ct:comment("Can't use POST nor DELETE with wrong password"),
  {basic_auth, {U, _}} = lists:keyfind(basic_auth, 1, Config),
  Headers4 = #{basic_auth => {U, "not-password"}},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers4),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/wrong-token", Headers4),

  ct:comment("Sessions can only be deleted by their user"),
  [_, {User2Name, _} | _] = application:get_env(sr_test, users, []),
  SessionId =
    sr_sessions:uri_path(sumo:persist(sr_sessions, sr_sessions:new(User2Name))),
  ForbiddenUri = binary_to_list(<<"/sessions/", SessionId/binary>>),
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  Headers5 = #{basic_auth => BasicAuth},
  #{status_code := 403} =
    sr_test_utils:api_call(delete, ForbiddenUri, Headers5),

  {comment, ""}.

-spec invalid_headers(sr_test_utils:config()) -> {comment, string()}.
invalid_headers(Config) ->
  Uri = "/sessions",
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  InvalidAccept = #{ basic_auth => BasicAuth
                   , <<"content-type">> => <<"application/json">>
                   , <<"accept">> => <<"text/html">>
                   },

  ct:comment("Agent must accept json for POST"),
  #{status_code := 406} =
    sr_test_utils:api_call(post, Uri, InvalidAccept, <<>>),

  {comment, ""}.
