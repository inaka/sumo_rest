%%% @doc Base GET|PUT|DELETE /[entity]s/:id implementation
-module(sr_single_entity_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , allowed_methods/2
          , content_types_provided/2
          , content_types_accepted/2
          , announce_req/2
          ]
        }]).

-export([ rest_init/2
        , resource_exists/2
        , handle_get/2
        , handle_put/2
        , handle_patch/2
        , delete_resource/2
        , id_from_binding/3 % exported only for test coverage
        ]).

-type options() :: #{ path => string()
                    , model => atom()
                    , verbose => boolean()
                    }.
-type state() :: #{ opts => options()
                  , id => binary()
                  , entity => sumo:user_doc()
                  , module => module()
                  , _ => _
                  }.
-export_type([state/0, options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Announces the Req and moves on.
%%      It extracts the <code>:id</code> binding from the Req and leaves it in
%%      the <code>id</code> key in the state.
%% @see cowboy_rest:rest_init/2
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  Req1 = announce_req(Req, Opts),
  #{model := Model} = Opts,
  Module = sumo_config:get_prop_value(Model, module),
  {Id, Req2} = cowboy_req:binding(id, Req1),
  IdType = sumo_internal:id_field_type(Model),
  ActualId = id_from_binding(IdType, Id, Module),
  {ok, Req2, #{opts => Opts, id => ActualId, module => Module}}.

%% @doc Verifies if there is an entity with the given <code>id</code>.
%%      The provided id must be the value for the id field in
%%      <strong>SumoDb</strong>. If the entity is found, it's kept in the
%%      state.
%% @see cowboy_rest:resource_exists/2
%% @see sumo:find/2
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  #{opts := #{model := Model}, id := Id} = State,
  case sumo:find(Model, Id) of
    notfound -> {false, Req, State};
    Entity -> {true, Req, State#{entity => Entity}}
  end.

%% @doc Renders the found entity.
%% @see resource_exists/2
-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{entity := Entity, module := Module} = State,
  ResBody = sr_json:encode(Module:to_json(Entity)),
  {ResBody, Req, State}.

%% @doc Updates the found entity.
%%      To parse the body, it uses <code>update/2</code> from the
%%      <code>model</code> provided in the options.
%% @see resource_exists/2
-spec handle_patch(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_patch(Req, #{entity := Entity} = State) ->
  #{module := Module} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    persist(Module:update(Entity, Json), Req1, State)
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.

%% @doc Updates the entity if found, otherwise it creates a new one.
%%      To parse the body, it uses either <code>update/2</code> or
%%      <code>from_json/2</code> (if defined) or <code>from_json/1</code>
%%      from the <code>model</code> provided in the options.
%% @see resource_exists/2
-spec handle_put(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_put(Req, #{entity := Entity} = State) ->
  #{module := Module} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    persist(Module:update(Entity, Json), Req1, State)
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end;
handle_put(Req, #{id := Id} = State) ->
  #{module := Module} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    persist(from_json(Module, Id, Json), Req1, State)
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.

%% @doc Deletes the found entity.
%% @see resource_exists/2
-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  #{opts := #{model := Model}, id := Id} = State,
  Result = sumo:delete(Model, Id),
  {Result, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
from_json(Module, Id, Json) ->
  try Module:from_json(Id, Json)
  catch
    _:undef -> Module:from_json(Json)
  end.

persist({error, Reason}, Req, State) ->
  Req1 = cowboy_req:set_resp_body(sr_json:error(Reason), Req),
  {false, Req1, State};
persist({ok, Entity}, Req1, State) ->
  #{opts := #{model := Model}, module := Module} = State,
  PersistedEntity = sumo:persist(Model, Entity),
  ResBody = sr_json:encode(Module:to_json(PersistedEntity)),
  Req2 = cowboy_req:set_resp_body(ResBody, Req1),
  {true, Req2, State}.

-spec id_from_binding(sumo:field_type(), binary(), atom()) -> term().
id_from_binding(binary, Id, _) ->
  Id;
id_from_binding(string, Id, _) ->
  binary_to_list(Id);
id_from_binding(integer, BinaryId, _) ->
  try binary_to_integer(BinaryId) of
    Id -> Id
  catch
    error:badarg -> -1
  end;
id_from_binding(_, BinaryId, Module) ->
  case erlang:function_exported(Module, id_from_binding, 1) of
    false -> BinaryId;
    true -> Module:id_from_binding(BinaryId)
  end.
