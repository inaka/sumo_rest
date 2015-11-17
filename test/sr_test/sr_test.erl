%%% @doc Main Application module
-module(sr_test).

-behaviour(application).

-export([ start/2
        , start_phase/3
        , stop/1
        ]).

-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_StartType, _Args) -> io:format("Hi!!~n"), {ok, self()}.

%% @todo revert cowboy-swagger workaround
%%       once https://github.com/inaka/cowboy-swagger/issues/26 is fixed
-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  ok = hack_cowboy_swagger_path(),

  Handlers =
    [ sr_entities_handler
    , cowboy_swagger_handler
    ],
  Routes = trails:trails(Handlers),
  trails:store(Routes),
  Dispatch = trails:single_host_compile(Routes),

  TransOpts = [{port, 4891}],
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  case cowboy:start_http(sr_test_server, 1, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.

-spec stop(atom()) -> ok.
stop(_State) -> ok.

%% @doc Properly sets cowboy swagger priv path.
%% @hack Waiting for https://github.com/inaka/cowboy-swagger/issues/26
%% @todo revert cowboy-swagger workaround
%%       once https://github.com/inaka/cowboy-swagger/issues/26 is fixed
-spec hack_cowboy_swagger_path() -> ok.
hack_cowboy_swagger_path() ->
  CowboySwaggerPriv =
    case code:priv_dir(cowboy_swagger) of
      {error, bad_name} ->
        filename:join(
          [ filename:dirname(code:which(cowboy_swagger_handler))
          , ".."
          , "priv"
          ]);
      Path -> Path
    end,
  StaticFiles = filename:join(CowboySwaggerPriv, "swagger"),
  application:set_env(cowboy_swagger, static_files, StaticFiles),
  ok.
