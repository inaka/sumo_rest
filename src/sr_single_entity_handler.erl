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
        , id_from_binding_internal/2 % exported only for test coverage
        ]).

-type options() :: sr_state:options().
-type state() :: sr_state:state().
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
  ActualId = id_from_binding(Id, Model, Module),
  State = sr_state:new(Opts, Module),
  State1 = sr_state:id(State, ActualId),
  {ok, Req2, State1}.

%% @doc Verifies if there is an entity with the given <code>id</code>.
%%      The provided id must be the value for the id field in
%%      <strong>SumoDb</strong>. If the entity is found, it's kept in the
%%      state.
%% @see cowboy_rest:resource_exists/2
%% @see sumo:find/2
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  Id = sr_state:id(State),
  #{model := Model} = sr_state:opts(State),
  case sumo:fetch(Model, Id) of
    notfound -> {false, Req, State};
    Entity -> {true, Req, sr_state:entity(State, Entity)}
  end.

%% @doc Renders the found entity.
%% @see resource_exists/2
-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  Entity = sr_state:entity(State),
  Module = sr_state:module(State),
  ResBody = sr_json:encode(Module:to_json(Entity)),
  {ResBody, Req, State}.

%% @doc Updates the found entity.
%%      To parse the body, it uses <code>update/2</code> from the
%%      <code>model</code> provided in the options.
%% @see resource_exists/2
-spec handle_patch(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
handle_patch(Req, State) ->
  Entity = sr_state:entity(State),
  Module = sr_state:module(State),
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
  {boolean() | halt, cowboy_req:req(), state()}.
handle_put(Req, State) ->
  try
    Module = sr_state:module(State),
    {SrRequest, Req1} = sr_request:from_cowboy(Req),
    Entity = case sr_state:entity(State) of
      undefined ->
        Context = #{req => SrRequest, state => State},
        build_entity(Context);
      OldEntity ->
        Json = sr_request:body(SrRequest),
        Module:update(OldEntity, Json)
    end,
    persist(Entity, Req1, State)
  catch
    _:badjson ->
      Req2 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req2, State}
  end.

%% @doc Deletes the found entity.
%% @see resource_exists/2
-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean() | halt, cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  Id = sr_state:id(State),
  #{model := Model} = sr_state:opts(State),
  Result = sumo:delete(Model, Id),
  {Result, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_entity(#{req := SrRequest, state := State} = Context) ->
  Module = sr_state:module(State),
  case erlang:function_exported(Module, from_ctx, 1) of
    false ->
      Id = sr_state:id(State),
      Json = sr_request:body(SrRequest),
      from_json(Module, Id, Json);
    true  ->
      Module:from_ctx(Context)
  end.

from_json(Module, Id, Json) ->
  try Module:from_json(Id, Json)
  catch
    _:undef -> Module:from_json(Json)
  end.

persist({error, Reason}, Req, State) ->
  Req1 = cowboy_req:set_resp_body(sr_json:error(Reason), Req),
  {false, Req1, State};
persist({ok, Entity}, Req1, State) ->
  Module = sr_state:module(State),
  #{model := Model} = sr_state:opts(State),
  PersistedEntity = sumo:persist(Model, Entity),
  ResBody = sr_json:encode(Module:to_json(PersistedEntity)),
  Req2 = cowboy_req:set_resp_body(ResBody, Req1),
  {true, Req2, State}.

-spec id_from_binding(binary(), atom(), atom()) -> term().
id_from_binding(Id, Model, Module) ->
  case erlang:function_exported(Module, id_from_binding, 1) of
    false -> id_from_binding_internal(Id, sumo_internal:id_field_type(Model));
    true -> Module:id_from_binding(Id)
  end.

-spec id_from_binding_internal(binary(), binary | string | integer) -> term().
id_from_binding_internal(Id, binary) ->
  Id;
id_from_binding_internal(Id, string) ->
  binary_to_list(Id);
id_from_binding_internal(BinaryId, integer) ->
  try binary_to_integer(BinaryId) of
    Id -> Id
  catch
    error:badarg -> -1
  end.
