-module(example_SUITE).

-mixin([{sr_test_SUITE, [method_not_allowed/1]}]).

-exports([init_per_suite/1]).

init_per_suite(Config) ->
  [{test_config, [#{ path => "/path"
                   , methods => [get, post]
                   , model => poke_pokemons
                   }]} | Config].