%%% @doc Elements Model
-module(sr_elements).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type key() :: binary().
-type value() :: binary() | iodata().

-opaque element() ::
  #{ key        => key()
   , value      => value()
   , created_at => calendar:datetime()
   , updated_at => calendar:datetime()
   }.

-export_type(
  [ element/0
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
  , location/2
  , id/1
  , update/2
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

-spec sumo_sleep(element()) -> sumo:model().
sumo_sleep(Element) -> Element.

-spec sumo_wakeup(sumo:model()) -> element().
sumo_wakeup(Element) -> Element.

-spec to_json(element()) -> sr_json:json().
to_json(Element) ->
  #{ key        => maps:get(key, Element)
   , value      => maps:get(value, Element)
   , created_at => sr_json:encode_date(maps:get(created_at, Element))
   , updated_at => sr_json:encode_date(maps:get(updated_at, Element))
   }.

-spec from_json(sumo_rest_doc:json()) -> {ok, element()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ key        => maps:get(<<"key">>, Json)
       , value      => maps:get(<<"value">>, Json)
       , created_at =>
          sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , updated_at =>
          sr_json:decode_date(maps:get(<<"updated_at">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(element(), sumo_rest_doc:json()) ->
  {ok, element()} | {error, iodata()}.
update(Element, Json) ->
  try
    NewValue = maps:get(<<"value">>, Json),
    UpdatedElement =
      Element#{value := NewValue, updated_at := calendar:universal_time()},
    {ok, UpdatedElement}
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.

-spec location(element(), sumo_rest_doc:path()) -> binary().
location(Element, Path) -> iolist_to_binary([Path, "/", key(Element)]).

-spec id(element()) -> key().
id(Element) -> key(Element).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(key(), value()) -> element().
new(Key, Value) ->
  Now = calendar:universal_time(),
  #{ key        => Key
   , value      => Value
   , created_at => Now
   , updated_at => Now
   }.

-spec key(element()) -> key().
key(#{key := Key}) -> Key.

-spec value(element()) -> value().
value(#{value := Value}) -> Value.

-spec updated_at(element()) -> calendar:datetime().
updated_at(#{updated_at := UpdatedAt}) -> UpdatedAt.
