%%% @doc Base GET|POST /[entities] implementation
-module(sr_entities_handler).

-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , resource_exists/2
        , content_types_accepted/2
        , content_types_provided/2
        , handle_get/2
        , handle_post/2
        ]).
-export([ announce_req/2
        , handle_post/3
        ]).

-type options() :: #{ path => string()
                    , model => atom()
                    , verbose => boolean()
                    }.
-type state() :: #{ opts => options()
                  , module => module()
                  }.
-export_type([state/0, options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Upgrades to cowboy_rest.
%%      Basically, just returns <code>{upgrade, protocol, cowboy_rest}</code>
%% @see cowboy_rest:init/3
-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @doc Announces the Req and moves on.
%%      If <code>verbose := true</code> in <code>Opts</code> for this handler
%%      prints out a line indicating that endpoint that was hit.
%% @see cowboy_rest:rest_init/2
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  Req1 = announce_req(Req, Opts),
  #{model := Model} = Opts,
  Module = sumo_config:get_prop_value(Model, module),
  {ok, Req1, #{opts => Opts, module => Module}}.

%% @doc Retrieves the list of allowed methods from Trails metadata.
%%      Parses the metadata associated with this path and returns the
%%      corresponding list of endpoints.
%% @see cowboy_rest:allowed_methods/2
-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  #{opts := #{path := Path}} = State,
  Trail = trails:retrieve(Path),
  Metadata = trails:metadata(Trail),
  Methods = [atom_to_method(Method) || Method <- maps:keys(Metadata)],
  {Methods, Req, State}.

%% @doc Returns <code>false</code> for POST, <code>true</code> otherwise.
%% @see cowboy_rest:resource_exists/2
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req1, State}.

%% @doc Always returns "application/json *" with <code>handle_post</code>.
%% @see cowboy_rest:content_types_accepted/2
%% @todo Use swagger's 'consumes' to auto-generate this if possible
%%       <a href="https://github.com/inaka/sumo_rest/issues/7">Issue</a>
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
    #{opts := #{path := Path}} = State,
    {Method, Req2} = cowboy_req:method(Req),
    try
        Trail = trails:retrieve(Path),
        Metadata = trails:metadata(Trail),
        AtomMethod = method_to_atom(Method),
        #{AtomMethod := #{consumes := Consumes}} = Metadata,
        Handler = compose_handler_name(AtomMethod),
        RetList = [{iolist_to_binary(X), Handler} || X <- Consumes],
        {RetList, Req2, State}
    catch
        _:_ ->
            {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}
    end.

%% @doc Always returns "application/json" with <code>handle_get</code>.
%% @see cowboy_rest:content_types_provided/2
%% @todo Use swagger's 'produces' to auto-generate this if possible
%%       <a href="https://github.com/inaka/sumo_rest/issues/7">Issue</a>
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    #{opts := #{path := Path}} = State,
    {Method, Req2} = cowboy_req:method(Req),
    try
        Trail = trails:retrieve(Path),
        Metadata = trails:metadata(Trail),
        AtomMethod = method_to_atom(Method),
        #{AtomMethod := #{produces := Produces}} = Metadata,
        Handler = compose_handler_name(AtomMethod),
        RetList = [{iolist_to_binary(X), Handler} || X <- Produces],
        {RetList, Req2, State}
    catch
        _:_ ->
            {[{<<"application/json">>, handle_get}], Req, State}
    end.

%% @doc Returns the list of all entities.
%%      Fetches the entities from <strong>SumoDB</strong> using the
%%      <code>model</code> provided in the options.
%% @todo Use query-string as filters.
%%       <a href="https://github.com/inaka/sumo_rest/issues/8">Issue</a>
-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{ opts := #{model := Model}
   , module := Module
   } = State,
  {Qs, Req1} = cowboy_req:qs_vals(Req),
  Conditions = [ {binary_to_atom(Name, unicode),
    Value} || {Name, Value} <- Qs ],
  Schema = sumo_internal:get_schema(Model),
  Fields = [ sumo_internal:field_name(Field) ||
      Field <- sumo_internal:schema_fields(Schema) ],
  CompareFun = fun({Name, _}) ->
    true =:= lists:member(Name, Fields)
  end,
  ValidConditions = lists:filter(CompareFun, Conditions),
  Entities = case ValidConditions of
    [] -> sumo:find_all(Model);
    _  -> sumo:find_by(Model, Conditions)
  end,
  Reply     = [Module:to_json(Entity) || Entity <- Entities],
  JSON      = sr_json:encode(Reply),
  {JSON, Req1, State}.

%% @doc Creates a new entity.
%%      To parse the body, it uses <code>from_json/2</code> from the
%%      <code>model</code> provided in the options.
-spec handle_post(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  #{module := Module} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    case Module:from_json(Json) of
      {error, Reason} ->
        Req2 = cowboy_req:set_resp_body(sr_json:error(Reason), Req1),
        {false, Req2, State};
      {ok, Entity} ->
        handle_post(Entity, Req1, State)
    end
  catch
    _:conflict ->
      {ok, Req3} =
        cowboy_req:reply(409, [], sr_json:error(<<"Duplicated entity">>), Req),
      {halt, Req3, State};
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.

%% @doc Persists a new entity.
%%      The body must have been parsed beforehand.
-spec handle_post(sumo:user_doc(), cowboy_req:req(), state()) ->
  {{true, binary()}, cowboy_req:req(), state()}.
handle_post(Entity, Req1, State) ->
  #{opts := #{model := Model, path := Path}, module := Module} = State,
  case erlang:function_exported(Module, id, 1) of
    false -> proceed;
    true ->
      Id = Module:id(Entity),
      case sumo:find(Model, Id) of
        notfound -> proceed;
        Duplicate ->
          error_logger:warning_msg(
            "Duplicated ~p with id ~p: ~p", [Model, Id, Duplicate]),
          throw(conflict)
      end
  end,
  PersistedEntity = sumo:persist(Model, Entity),
  ResBody = sr_json:encode(Module:to_json(PersistedEntity)),
  Req2 = cowboy_req:set_resp_body(ResBody, Req1),
  Location = Module:location(PersistedEntity, Path),
  {{true, Location}, Req2, State}.

%% @doc Announces the Req.
%%      If <code>verbose := true</code> in <code>Opts</code> for this handler
%%      prints out a line indicating that endpoint that was hit.
%% @see cowboy_rest:rest_init/2
-spec announce_req(cowboy_req:req(), options()) -> cowboy_req:req().
announce_req(Req, #{verbose := true}) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  _ = error_logger:info_msg("~s ~s", [Method, Path]),
  Req2;
announce_req(Req, _Opts) -> Req.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec atom_to_method(get|patch|put|post|delete) -> binary().
atom_to_method(get) -> <<"GET">>;
atom_to_method(patch) -> <<"PATCH">>;
atom_to_method(put) -> <<"PUT">>;
atom_to_method(post) -> <<"POST">>;
atom_to_method(delete) -> <<"DELETE">>.

-spec method_to_atom(binary()) -> atom().
method_to_atom(<<"GET">>) -> get;
method_to_atom(<<"PATCH">>) -> patch;
method_to_atom(<<"PUT">>) -> put;
method_to_atom(<<"POST">>) -> post;
method_to_atom(<<"DELETE">>) -> delete.

-spec compose_handler_name(get|patch|put|post) -> atom().
compose_handler_name(get) -> handle_get;
compose_handler_name(put) -> handle_put;
compose_handler_name(patch) -> handle_patch;
compose_handler_name(post) -> handle_post.
