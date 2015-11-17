%%% @doc Entities Model
-module(sr_entities).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type key() :: binary().
-type value() :: binary() | iodata().

-opaque entity() ::
  #{ key        => binary()
   , value      => binary()
   , created_at => calendar:datetime()
   , updated_at => calendar:datetime()
   }.

-export_type(
  [ entity/0
  , key/0
  , value/0
  ]).

-export(
  [ sumo_schema/0
  , sumo_wakeup/1
  , sumo_sleep/1
  ]).
-export(
  [ new/2
  , key/1
  , value/1
  , updated_at/1
  ]).
-export(
  [ to_json/1
  , from_json/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(key,        string,   [id, not_null])
    , sumo:new_field(value,      string,   [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    , sumo:new_field(updated_at, datetime, [not_null])
    ]).

-spec sumo_sleep(entity()) -> sumo:doc().
sumo_sleep(Entity) -> Entity.

-spec sumo_wakeup(sumo:doc()) -> entity().
sumo_wakeup(Entity) -> Entity.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(key(), value()) -> entity().
new(Key, Value) ->
  Now = calendar:universal_time(),
  #{ key        => Key
   , value      => Value
   , created_at => Now
   , updated_at => Now
   }.

-spec key(entity()) -> key().
key(#{key := Key}) -> Key.

-spec value(entity()) -> value().
value(#{value := Value}) -> Value.

-spec updated_at(entity()) -> calendar:datetime().
updated_at(#{updated_at := UpdatedAt}) -> UpdatedAt.

-spec to_json(entity()) -> sumo_rest_doc:json().
to_json(Entity) ->
  #{ key        => maps:get(key, Entity)
   , value      => maps:get(value, Entity)
   , created_at => sr_json:encode_date(maps:get(created_at, Entity))
   , updated_at => sr_json:encode_date(maps:get(updated_at, Entity))
   }.

-spec from_json(sumo_rest_doc:json()) -> entity().
from_json(Json) ->
  #{ key        => maps:get(key, Json)
   , value      => maps:get(value, Json)
   , created_at => sr_json:decode_date(maps:get(created_at, Json))
   , updated_at => sr_json:decode_date(maps:get(updated_at, Json))
   }.
