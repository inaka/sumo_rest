-module(sr_basic_SUITE).

-export([all/0]).
-export([types/1]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).

-spec types(sr_test_utils:config()) -> {comment, string()}.
types(_Config) ->
  ok = state1(#{any => value}),
  ok = state2(#{any => value}),
  
  {comment, ""}.

-spec state1(sr_entities_handler:state()) -> ok.
state1(_State) -> ok.

-spec state2(sr_single_entity_handler:state()) -> ok.
state2(_State) -> ok.
