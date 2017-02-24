%%% @doc echo_request model. This module is only for testing purposes
-module(sr_echo_request).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-opaque echo_request() ::
  #{ id       := undefined | binary()
   , headers  := any()
   , path     := any()
   , bindings := any()
   }.

-export_type([echo_request/0]).

-export(
  [ sumo_schema/0
  , sumo_wakeup/1
  , sumo_sleep/1
  ]).
-export(
  [ to_json/1
  , from_ctx/1
  , location/2
  , update/2
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(echo_request,
    [ sumo:new_field(id,       string, [id, not_null])
    , sumo:new_field(headers,  custom, [not_null])
    , sumo:new_field(path,     binary, [not_null])
    , sumo:new_field(bindings, custom, [not_null])
    ]).

-spec sumo_sleep(echo_request()) -> sumo:model().
sumo_sleep(EchoRequest) -> EchoRequest.

-spec sumo_wakeup(sumo:model()) -> echo_request().
sumo_wakeup(EchoRequest) -> EchoRequest.

-spec to_json(echo_request()) -> sr_json:json().
to_json(EchoRequest) ->
  #{ id       => maps:get(id, EchoRequest)
   , headers  => maps:from_list(maps:get(headers, EchoRequest))
   , path     => maps:get(path, EchoRequest)
   , bindings => maps:get(bindings, EchoRequest)
   }.

-spec update(echo_request(), sumo_rest_doc:json()) ->
  {ok, echo_request()} | {error, iodata()}.
update(EchoRequest, _Json) ->
  EchoRequest.

-spec location(echo_request(), sumo_rest_doc:path()) -> binary().
location(#{id := Id}, Path) ->
  iolist_to_binary([Path, "/", Id]).

-spec from_ctx(sumo_rest_doc:context()) -> {ok, echo_request()}.
from_ctx(#{req := Req, state := State}) ->
  {ok, #{ id       => sr_state:id(State)
        , headers  => sr_request:headers(Req)
        , path     => sr_request:path(Req)
        , bindings => sr_request:bindings(Req)
        }}.
