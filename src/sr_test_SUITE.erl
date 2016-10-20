-module(sr_test_SUITE).

-spec method_not_allowed(any()) -> ok.
method_not_allowed(Config) ->
  TestConfig = proplists:get_value(test_config, Config),
  #{path := Path, methods := AllowedMethods} = TestConfig,

  AllMethods = [get, post, delete, put, patch, head, options, connect],

  true = lists:all(fun(Method) ->
                     {409, _} = sr_test_utils:api_call(Method, Path)
                   end, AllMethods -- AllowedMethods),

  ok.