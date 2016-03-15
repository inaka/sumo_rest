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
        , invalid_parameters/1
        , conflict/1
        , location/1
        ]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).

-spec init_per_testcase(atom(), sr_test_utils:config()) ->
  sr_test_utils:config().
init_per_testcase(_, Config) ->
  _ = sumo:delete_all(sr_sessions),
  {U, P} = first_user(),
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
    sr_test_utils:api_call(post, "/sessions", Headers, #{agent => <<"a1">>}),
  #{ <<"id">>           := Session1Id
   , <<"token">>        := Token1
   , <<"agent">>        := <<"a1">>
   , <<"created_at">>   := CreatedAt1
   , <<"expires_at">>   := ExpiresAt1
   } = sr_json:decode(Body1),
  true = ExpiresAt1 >= CreatedAt1,

  ct:comment("Session ~s is there", [Session1Id]),
  [Session1] = sumo:find_all(sr_sessions),
  Token1 = sr_sessions:token(Session1),

  ct:comment("The session agent can be changed"),
  #{status_code := 200, body := Body2} =
    sr_test_utils:api_call(
      put, "/sessions/" ++ binary_to_list(Session1Id), Headers,
      #{agent => <<"a2">>}),
  #{ <<"id">>           := Session1Id
   , <<"agent">>        := <<"a2">>
   , <<"created_at">>   := CreatedAt1
   , <<"expires_at">>   := ExpiresAt2
   } = sr_json:decode(Body2),
  true = ExpiresAt2 >= ExpiresAt1,

  ct:comment("Still just one session"),
  [Session2] = sumo:find_all(sr_sessions),

  ct:comment("Another session can be created with no agent"),
  #{status_code := 201, body := Body3} =
    sr_test_utils:api_call(post, "/sessions", Headers, #{}),
  #{ <<"id">>           := Session3Id
   , <<"token">>        := Token3
   , <<"agent">>        := null
   , <<"created_at">>   := CreatedAt3
   , <<"expires_at">>   := ExpiresAt3
   } = sr_json:decode(Body3),
  true = ExpiresAt3 >= CreatedAt3,
  ct:pal("~p < ~p ?", [ExpiresAt3, ExpiresAt2]),
  true = ExpiresAt3 >= ExpiresAt2,
  true = CreatedAt3 >= CreatedAt1,
  case Session3Id of
    Session1Id -> ct:fail("Duplicated Session Id: ~p", [Session1Id]);
    Session3Id -> ok
  end,
  case Token3 of
    Token1 -> ct:fail("Duplicated token: ~p", [Token1]);
    Token3 -> ok
  end,

  ct:comment("There are 2 sessions"),
  [Session3] = sumo:find_all(sr_sessions) -- [Session2],

  ct:comment("Session2 is deleted"),
  Uri4 = binary_to_list(<<"/sessions/", Session1Id/binary>>),
  #{status_code := 204} = sr_test_utils:api_call(delete, Uri4, Headers),

  ct:comment("One session again"),
  [Session3] = sumo:find_all(sr_sessions),

  ct:comment("DELETE is not idempotent"),
  Uri5 = binary_to_list(<<"/sessions/", Session3Id/binary>>),
  #{status_code := 204} = sr_test_utils:api_call(delete, Uri5, Headers),
  #{status_code := 404} = sr_test_utils:api_call(delete, Uri5, Headers),

  ct:comment("There are no sessions"),
  [] = sumo:find_all(sr_sessions),

  {comment, ""}.

-spec invalid_auth(sr_test_utils:config()) -> {comment, string()}.
invalid_auth(Config) ->
  Uri = "/sessions",

  ct:comment("Can't use POST, PUT nor DELETE without auth"),
  #{status_code := 401} = sr_test_utils:api_call(post, Uri),
  #{status_code := 401} = sr_test_utils:api_call(put, Uri ++ "/noauth"),
  #{status_code := 401} = sr_test_utils:api_call(delete, Uri ++ "/noauth"),

  ct:comment("Can't use POST, PUT nor DELETE without Basic auth"),
  Headers1 = #{<<"Authorization">> => <<"Bearer iSnotAGoodThing">>},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers1),
  #{status_code := 401} =
    sr_test_utils:api_call(put, Uri ++ "/other", Headers1),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/other", Headers1),

  ct:comment("Can't use POST, PUT nor DELETE with broken Basic auth"),
  Headers2 = #{<<"Authorization">> => <<"Basic ThisIsNotBase64">>},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers2),
  #{status_code := 401} =
    sr_test_utils:api_call(put, Uri ++ "/broken", Headers2),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/broken", Headers2),

  ct:comment("Can't use POST, PUT nor DELETE with wrong user"),
  Headers3 = #{basic_auth => {"not-user", "pwd"}},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers3),
  #{status_code := 401} =
    sr_test_utils:api_call(put, Uri ++ "/not-user", Headers3),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/not-user", Headers3),

  ct:comment("Can't use POST, PUT nor DELETE with wrong password"),
  {basic_auth, {U, _}} = lists:keyfind(basic_auth, 1, Config),
  Headers4 = #{basic_auth => {U, "not-password"}},
  #{status_code := 401} = sr_test_utils:api_call(post, Uri, Headers4),
  #{status_code := 401} =
    sr_test_utils:api_call(put, Uri ++ "/wrong-token", Headers4),
  #{status_code := 401} =
    sr_test_utils:api_call(delete, Uri ++ "/wrong-token", Headers4),

  ct:comment("Sessions can only be modified or deleted by their user"),
  [_, {User2Name, _} | _] = application:get_env(sr_test, users, []),
  SessionId =
    sr_sessions:unique_id(sumo:persist( sr_sessions
                                      , sr_sessions:new(User2Name)
                                      )),
  ForbiddenUri = binary_to_list(<<"/sessions/", SessionId/binary>>),
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  Headers5 = #{basic_auth => BasicAuth},
  #{status_code := 403} = sr_test_utils:api_call(put, ForbiddenUri, Headers5),
  #{status_code := 403} =
    sr_test_utils:api_call(delete, ForbiddenUri, Headers5),

  {comment, ""}.

-spec invalid_headers(sr_test_utils:config()) -> {comment, string()}.
invalid_headers(Config) ->
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  NoHeaders = #{basic_auth => BasicAuth},
  InvalidHeaders = #{ basic_auth => BasicAuth
                    , <<"content-type">> => <<"text/plain">>
                    },
  InvalidAccept = #{ basic_auth => BasicAuth
                   , <<"content-type">> => <<"application/json">>
                   , <<"accept">> => <<"text/html">>
                   },

  {User, _} = first_user(),
  SessionId =
    sr_sessions:unique_id(sumo:persist(sr_sessions, sr_sessions:new(User))),
  SessionUri = binary_to_list(<<"/sessions/", SessionId/binary>>),

  ct:comment("content-type must be provided for POST and PUT"),
  #{status_code := 415} =
    sr_test_utils:api_call(post, "/sessions", NoHeaders, <<>>),
  #{status_code := 415} =
    sr_test_utils:api_call(put, SessionUri, NoHeaders, <<>>),

  ct:comment("content-type must be JSON for POST and PUT"),
  #{status_code := 415} =
    sr_test_utils:api_call(post, "/sessions", InvalidHeaders, <<>>),
  #{status_code := 415} =
    sr_test_utils:api_call(put, SessionUri, InvalidHeaders, <<>>),

  ct:comment("Agent must accept json for POST and PUT"),
  #{status_code := 406} =
    sr_test_utils:api_call(post, "/sessions", InvalidAccept, <<>>),
  #{status_code := 406} =
    sr_test_utils:api_call(put, SessionUri, InvalidAccept, <<>>),

  {comment, ""}.

-spec invalid_parameters(sr_test_utils:config()) -> {comment, string()}.
invalid_parameters(Config) ->
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  Headers = #{ basic_auth => BasicAuth
              , <<"content-type">> => <<"application/json">>
              },

  {User, _} = first_user(),
  SessionId =
    sr_sessions:unique_id(sumo:persist(sr_sessions, sr_sessions:new(User))),
  SessionUri = binary_to_list(<<"/sessions/", SessionId/binary>>),

  ct:comment("Empty or broken parameters are reported"),
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/sessions", Headers, <<>>),
  #{status_code := 400} =
    sr_test_utils:api_call(put, SessionUri, Headers, <<>>),
  #{status_code := 400} =
    sr_test_utils:api_call(post, "/sessions", Headers, <<"{">>),
  #{status_code := 400} =
    sr_test_utils:api_call(put, SessionUri, Headers, <<"{">>),

  {comment, ""}.

-spec conflict(sr_test_utils:config()) -> {comment, string()}.
conflict(Config) ->
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  Headers = #{ basic_auth => BasicAuth
              , <<"content-type">> => <<"application/json">>
              },

  ct:comment("Can't update unexisting session"),
  #{status_code := 409} =
    sr_test_utils:api_call(put, "/sessions/notfound", Headers, #{}),
  {comment, ""}.

-spec location(st_test_utils:config()) -> {comment, string()}.
location(Config) ->
  {basic_auth, BasicAuth} = lists:keyfind(basic_auth, 1, Config),
  Headers = #{ basic_auth => BasicAuth
             , <<"content-type">> => <<"application/json">>
             },

  ct:comment("A session is created"),
  #{status_code := 201, body := Body1, headers := ResponseHeaders} =
    sr_test_utils:api_call(post, "/sessions", Headers, #{agent => <<"a1">>}),
  #{ <<"id">>           := Session1Id
   , <<"token">>        := _Token1
   , <<"agent">>        := <<"a1">>
   , <<"created_at">>   := _CreatedAt1
   , <<"expires_at">>   := _ExpiresAt1
   } = sr_json:decode(Body1),
  ct:comment("and its location header is set correctly"),
  Location = proplists:get_value(<<"location">>, ResponseHeaders),
  Location = <<"/sessions/", Session1Id/binary>>,

  {comment, ""}.

%% @private
first_user() ->
  [{U, P}|_] = application:get_env(sr_test, users, []),
  {U, P}.
