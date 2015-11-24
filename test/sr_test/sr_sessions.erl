%%% @doc Sessions Model
-module(sr_sessions).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type id() :: string().
-type token() :: binary().
-type user() :: string().

-opaque session() ::
  #{ id => undefined | id()
   , token => token()
   , user => user()
   , created_at => calendar:datetime()
   , expires_at => calendar:datetime()
   }.

-export_type(
  [ session/0
  , id/0
  , token/0
  ]).

-export(
  [ sumo_schema/0
  , sumo_wakeup/1
  , sumo_sleep/1
  ]).
-export(
  [ new/1
  , id/1
  , token/1
  , user/1
  , expires_at/1
  ]).
-export(
  [ to_json/1
  , from_json/1
  , uri_path/1
  , from_json/2
  , update/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,          string,   [id, not_null])
    , sumo:new_field(token,       string,   [not_null])
    , sumo:new_field(user,        string,   [not_null])
    , sumo:new_field(created_at,  datetime, [not_null])
    , sumo:new_field(expires_at,  datetime, [not_null])
    ]).

-spec sumo_sleep(session()) -> sumo:doc().
sumo_sleep(Session) -> Session.

-spec sumo_wakeup(sumo:doc()) -> session().
sumo_wakeup(Session) -> Session.

-spec to_json(session()) -> sumo_rest_doc:json().
to_json(Session) ->
  #{ id =>
      case maps:get(id, Session) of
        undefined -> null;
        Id -> list_to_binary(Id)
      end
   , token => maps:get(token, Session)
   , created_at => sr_json:encode_date(maps:get(created_at, Session))
   , expires_at => sr_json:encode_date(maps:get(expires_at, Session))
   }.

-spec from_json(binary(), sumo_rest_doc:json()) ->
  {ok, session()} | {error, iodata()}.
from_json(Id, Json) -> from_json(Json#{<<"id">> => Id}).

-spec from_json(sumo_rest_doc:json()) -> {ok, session()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ id =>
          case maps:get(<<"id">>, Json, null) of
            null -> undefined;
            Id -> binary_to_list(Id)
          end
       , token => maps:get(<<"token">>, Json)
       , created_at =>
          sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , expires_at =>
          sr_json:decode_date(maps:get(<<"expires_at">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(session(), sumo_rest_doc:json()) -> no_return().
update(_Session, _Json) -> throw(should_not_update_session).

-spec uri_path(session()) -> binary().
uri_path(Session) -> list_to_binary(id(Session)).

-spec id(session()) -> undefined | id().
id(#{id := Id}) -> Id.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(user()) -> session().
new(User) ->
  Now = calendar:universal_time(),
  ExpiresAt =
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(Now) + 60000),
  #{ id         => undefined
   , user       => User
   , token      => generate_token()
   , created_at => Now
   , expires_at => ExpiresAt
   }.

-spec token(session()) -> token().
token(#{token := Token}) -> Token.

-spec user(session()) -> user().
user(#{user := User}) -> User.

-spec expires_at(session()) -> calendar:datetime().
expires_at(#{expires_at := ExpiresAt}) -> ExpiresAt.

generate_token() -> base64:encode(crypto:strong_rand_bytes(32)).
