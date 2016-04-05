%%% @doc Base GET|PUT|DELETE /[entity]s/:id implementation
-module(sr_single_entity_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/2
          , allowed_methods/2
          , content_types_provided/2
          , announce_req/2
          ]
        }]).

-export([ resource_exists/2
        , content_types_accepted/2
        , handle_get/2
        , handle_put/2
        , handle_patch/2
        , delete_resource/2
        ]).

-type options() :: #{ path => string()
                    , model => module()
                    , verbose => boolean()
                    }.
-type state() :: #{ opts => options()
                  , id => binary()
                  , entity => sumo:user_doc()
                  }.
-export_type([state/0, options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%% @doc Always returns "application/json *".
%%      The function depends on the request method, it can be
%% <ul>
%%    <li> <code>handle_put</code> </li>
%%    <li> <code>handle_patch</code> </li>
%% </ul>
%% @see cowboy_rest:content_types_accepted/2
%% @todo Use swagger's 'consumes' to auto-generate this if possible.
%%       <a href="https://github.com/inaka/sumo_rest/issues/7">Issue</a>
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  Method = cowboy_req:method(Req),
  Function = method_function(Method),
  {[{{<<"application">>, <<"json">>, '*'}, Function}], Req, State}.

%% @doc Renders the found entity.
%% @see resource_exists/2
-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{opts := #{model := Model}, entity := Entity} = State,
  ResBody = sr_json:encode(Model:to_json(Entity)),
  {ResBody, Req, State}.

%% @doc Updates the found entity.
%%      To parse the body, it uses <code>update/2</code> from the
%%      <code>model</code> provided in the options.
%% @see resource_exists/2
-spec handle_patch(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_patch(Req, #{entity := Entity} = State) ->
  #{opts := #{model := Model}} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    persist(Model:update(Entity, Json), Req1, State)
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
  #{opts := #{model := Model}} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    persist(Model:update(Entity, Json), Req1, State)
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end;
handle_put(Req, #{id := Id} = State) ->
  #{opts := #{model := Model}} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    persist(from_json(Model, Id, Json), Req1, State)
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
from_json(Model, Id, Json) ->
  try Model:from_json(Id, Json)
  catch
    _:undef -> Model:from_json(Json)
  end.

persist({error, Reason}, Req, State) ->
  Req1 = cowboy_req:set_resp_body(Reason, Req),
  {false, Req1, State};
persist({ok, Entity}, Req1, State) ->
  #{opts := #{model := Model}} = State,
  PersistedEntity = sumo:persist(Model, Entity),
  ResBody = sr_json:encode(Model:to_json(PersistedEntity)),
  Req2 = cowboy_req:set_resp_body(ResBody, Req1),
  {true, Req2, State}.

-spec method_function(binary()) -> atom().
method_function(<<"PUT">>) -> handle_put;
method_function(<<"PATCH">>) -> handle_patch.
