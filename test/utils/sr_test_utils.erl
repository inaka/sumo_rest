%%% @doc Test Utilities
-module(sr_test_utils).

-type config() :: [{atom(), term()}].
-export_type([config/0]).

-export([ application_processes/1
        , fail/1
        , fail/2
        ]).
-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).
-export([ api_call/2
        , api_call/3
        , api_call/4
        , api_call/5
        ]).

-spec application_processes(atom()) -> [pid()].
application_processes(App) ->
  [Pid || {running, RunningApps} <- application:info()
        , {AppName, Pid} <- RunningApps
        , AppName =:= App
        ].

-spec fail(_) -> no_return().
fail(_) -> throw(an_expected_error).

-spec fail(_, _) -> no_return().
fail(_, _) -> throw(an_expected_error).

-spec all(atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite],
  [F || {F, 1} <- Module:module_info(exports)] -- ExcludedFuns.

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(sr_test),
  {ok, _} = shotgun:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = application:stop(sr_test),
  ok = shotgun:stop(),
  Config.

-spec api_call(atom(), string()) -> #{}.
api_call(Method, Uri) ->
  api_call(Method, Uri, #{}).

-spec api_call(atom(), string(), #{}) -> #{}.
api_call(Method, Uri, Headers) ->
  api_call(Method, Uri, Headers, []).

-spec api_call(atom(), string(), #{}, #{} | iodata()) -> #{}.
api_call(Method, Uri, Headers, Body) ->
  api_call(Method, Uri, Headers, #{}, Body).

-spec api_call(atom(), string(), #{}, #{}, #{}|iodata()) -> #{}.
api_call(Method, Uri, Headers, Opts, Body) when is_map(Body) ->
  api_call(Method, Uri, Headers, Opts, sr_json:encode(Body));
api_call(Method, Uri, Headers, Opts, Body) ->
  {ok, Pid} = shotgun:open("localhost", 4891),
  FullOpts = maps:merge(#{timeout => 30000}, Opts),
  try
    case shotgun:request(Pid, Method, Uri, Headers, Body, FullOpts) of
      {ok, Response} -> Response;
      {error, Reason} -> {error, Reason}
    end
  after
    shotgun:close(Pid)
  end.
