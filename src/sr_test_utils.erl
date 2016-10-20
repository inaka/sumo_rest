-module(sr_test_utils).

-export([basic_tests/1, advanced_tests/1]).
-export([api_call/2, api_call/3, api_call/4]).

%%==============================================================================
%% API
%%==============================================================================
-spec basic_tests([module()]) -> ok.
basic_tests(Handlers) ->
  % Clean up all the repos
  ct:comment("Clean up all the repos"),
  _ = [sumo:delete_all(sumo_internal:schema_name(Handler:sumo_schema())) ||
       Handler <- Handlers],

  % Wrong content types
  ok.

-spec advanced_tests([module()]) -> ok.
advanced_tests(Handlers) ->
  ok.

api_call(Method, Path) ->
  api_call(Method, Path, []).

api_call(Method, Path, Hdrs) when is_list(Hdrs) ->
  api_call(Method, Path, Hdrs, <<>>).

api_call(Method, Path, Hdrs, Json) when is_map(Json) ->
  api_call(Method, Path, Hdrs, jsx:encode(Json));
api_call(Method, Path, Headers, Body) ->
  {ok, Application} = application:get_application(),
  Port = integer_to_binary(application:get_env(Application, http_port, 8080)),
  BinPath = iolist_to_binary(Path),
  Url = <<"http://localhost:", Port/binary, BinPath/binary>>,
  ct:log("~p ~p -d '~p'", [Method, Url, Body]),
  try hackney:request(Method, Url, Headers, Body) of
    {ok, Status, _ResponseHeaders} -> Status;
    {ok, Status, _ResponseHeaders, ClientRef} ->
      case hackney:body(ClientRef) of
        {ok, <<>>} -> Status;
        {ok, ResponseBody} ->
          ct:log(">>> ~p", [ResponseBody]),
          {Status, maybe_decode(ResponseBody)}
      end;
    {error, Error} ->
      ct:fail("Couldnt ~p ~p: ~p", [Method, Path, Error])
  catch
    _:X ->
      ct:log("Error: ~p; Stack: ~p", [X, erlang:get_stacktrace()]),
      ct:fail("Couldnt ~p ~p: ~p", [Method, Path, X])
  end.

%% @todo remove once inaka/sumo_rest#44 is fixed
maybe_decode(RespBody) ->
  try jsx:decode(RespBody, [return_maps])
  catch
    _:_ -> RespBody
  end.