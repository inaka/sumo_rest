%%% @doc Main Application module
-module(sr_test).

-behaviour(application).

-export([ start/2
        , start_phase/3
        , stop/1
        ]).

-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_StartType, _Args) ->
  _ = application:stop(lager),
  ok = application:stop(sasl),
  {ok, _} = application:ensure_all_started(sasl),
  {ok, self()}.

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  Handlers =
    [ sr_elements_handler
    , sr_single_element_handler
    , sr_sessions_handler
    , sr_single_session_handler
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
