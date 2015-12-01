%%% @doc Base GET|POST /[entity]s implementation
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
                    , model => module()
                    , verbose => boolean()
                    }.
-type state() :: #{ opts => options()
                  }.
-export_type([state/0, options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  Req1 = announce_req(Req, Opts),
  {ok, Req1, #{opts => Opts}}.

-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  #{opts := #{path := Path}} = State,
  #{metadata := Metadata} = trails:retrieve(Path),
  Methods = [atom_to_method(Method) || Method <- maps:keys(Metadata)],
  {Methods, Req, State}.

-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req1, State}.

%% @todo Use swagger's 'consumes' to auto-generate this if possible
%% @see https://github.com/inaka/sumo_rest/issues/7
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

%% @todo Use swagger's 'produces' to auto-generate this if possible
%% @see https://github.com/inaka/sumo_rest/issues/7
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
  {[{<<"application/json">>, handle_get}], Req, State}.

-spec handle_get(cowboy_req:req(), state()) ->
  {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  #{opts := #{model := Model}} = State,
  Entities  = sumo:find_all(Model),
  Reply     = [Model:to_json(Entity) || Entity <- Entities],
  JSON      = sr_json:encode(Reply),
  {JSON, Req, State}.

-spec handle_post(cowboy_req:req(), state()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  #{opts := #{model := Model}} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    case Model:from_json(Json) of
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

-spec handle_post(sumo:user_doc(), cowboy_req:req(), state()) ->
  {{true, binary()}, cowboy_req:req(), state()}.
handle_post(Entity, Req1, State) ->
  #{opts := #{model := Model, path := Path}} = State,
  case erlang:function_exported(Model, id, 1) of
    false -> proceed;
    true ->
      Id = Model:id(Entity),
      case sumo:find(Model, Id) of
        notfound -> proceed;
        Duplicate ->
          error_logger:warning_msg(
            "Duplicated ~p with id ~p: ~p", [Model, Id, Duplicate]),
          throw(conflict)
      end
  end,
  PersistedEntity = sumo:persist(Model, Entity),
  ResBody = sr_json:encode(Model:to_json(PersistedEntity)),
  Req2 = cowboy_req:set_resp_body(ResBody, Req1),
  Location = iolist_to_binary([Path, Model:uri_path(PersistedEntity)]),
  {{true, Location}, Req2, State}.

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
