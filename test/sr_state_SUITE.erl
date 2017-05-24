-module(sr_state_SUITE).

-export([all/0]).
-export([state/1]).

-spec all() -> [atom()].
all() -> sr_test_utils:all(?MODULE).


-spec state(sr_test_utils:config()) -> {comment, string()}.
state(_Config) ->
  ct:comment("Testing sr_state functions"),
  Module = fake_module_name,
  Path = "/path",
  Opts = #{model => value1, path => Path},
  SrState = sr_state:new(Opts, Module),
  Module = sr_state:module(SrState),
  Path = sr_state:path(SrState),
  Opts = sr_state:opts(SrState),
  undefined = sr_state:id(SrState),
  Id = <<"myId">>,
  SrState2 = sr_state:id(SrState, Id),
  Id = sr_state:id(SrState2),
  SrState3 = sr_state:set(key, value, SrState2),
  value = sr_state:retrieve(key, SrState3, not_found),
  SrState4 = sr_state:remove(key, SrState3),
  not_found = sr_state:retrieve(key, SrState4, not_found),
  Entity = #{},
  undefined = sr_state:entity(SrState4),
  SrState5 = sr_state:entity(SrState4, Entity),
  Entity = sr_state:entity(SrState5),

  {comment, ""}.
