%%% @doc Sessions Model
-module(sr_sessions).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type id() :: binary().
-type token() :: binary().
-type user() :: binary().
-type agent() :: binary().

-opaque session() ::
  #{ id => undefined | id()
   , token => token()
   , agent => undefined | agent()
   , user => user()
   , created_at => calendar:datetime()
   , expires_at => calendar:datetime()
   }.

-export_type(
  [ session/0
  , id/0
  , token/0
  , agent/0
  ]).

-export(
  [ sumo_schema/0
  , sumo_wakeup/1
  , sumo_sleep/1
  ]).
-export(
  [ new/1
  , unique_id/1
  , token/1
  , user/1
  , user/2
  , expires_at/1
  ]).
-export(
  [ to_json/1
  , from_json/1
  , from_json/2
  , location/2
  , update/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,          binary,   [id, not_null])
    , sumo:new_field(token,       binary,   [not_null])
    , sumo:new_field(agent,       binary,   [])
    , sumo:new_field(user,        binary,   [not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    , sumo:new_field(expires_at,  datetime, [not_null])
    ]).

-spec sumo_sleep(session()) -> sumo:doc().
sumo_sleep(Session) -> Session.

-spec sumo_wakeup(sumo:doc()) -> session().
sumo_wakeup(Session) -> Session.

-spec to_json(session()) -> sr_json:json().
to_json(Session) ->
  #{ id => sr_json:encode_null(maps:get(id, Session))
   , token => maps:get(token, Session)
   , agent => sr_json:encode_null(maps:get(agent, Session))
   , created_at => sr_json:encode_date(maps:get(created_at, Session))
   , expires_at => sr_json:encode_date(maps:get(expires_at, Session))
   }.

-spec from_json(id(), sumo_rest_doc:json()) ->
  {ok, session()} | {error, iodata()}.
from_json(Id, Json) -> from_json(Json#{<<"id">> => Id}).

-spec from_json(sumo_rest_doc:json()) -> {ok, session()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  ExpiresAt = sr_json:encode_date(expires_at()),
  try
    { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , token => maps:get(<<"token">>, Json, generate_token())
       , agent => sr_json:decode_null(maps:get(<<"agent">>, Json, null))
       , created_at =>
          sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , expires_at =>
          sr_json:decode_date(maps:get(<<"expires_at">>, Json, ExpiresAt))
       }
    }
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(session(), sumo_rest_doc:json()) ->
  {ok, session()} | {error, iodata()}.
update(Session, Json) ->
  #{id := SessionId} = Session,
  case from_json(SessionId, Json) of
    {error, Reason} -> {error, Reason};
    {ok, Updates} ->
      UpdatedSession = maps:merge(Session, Updates),
      {ok, UpdatedSession#{ expires_at => expires_at()
                          , token => generate_token()
                          }}
  end.

-spec location(session(), sumo_rest_doc:path()) -> binary().
location(Session, Path) -> iolist_to_binary([Path, "/", unique_id(Session)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(user()) -> session().
new(User) ->
  Now = calendar:universal_time(),
  #{ id         => undefined
   , user       => User
   , agent      => undefined
   , token      => generate_token()
   , created_at => Now
   , expires_at => expires_at()
   }.

unique_id(#{id := Id}) -> Id.

-spec token(session()) -> token().
token(#{token := Token}) -> Token.

-spec user(session()) -> user().
user(#{user := User}) -> User.

-spec user(session(), user()) -> session().
user(Session, User) -> Session#{user => User}.

-spec expires_at(session()) -> calendar:datetime().
expires_at(#{expires_at := ExpiresAt}) -> ExpiresAt.

generate_token() -> base64:encode(crypto:strong_rand_bytes(32)).

expires_at() ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(
      calendar:universal_time()) + 60000).
