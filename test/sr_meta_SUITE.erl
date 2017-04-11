-module(sr_meta_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ ktn_meta_SUITE
       , [ all/0
         , dialyzer/1
         , xref/1
         , elvis/1
         ]
       }]).

-export([init_per_suite/1]).

init_per_suite(Config) ->
  DWs = [error_handling, race_conditions, unmatched_returns, unknown],
  [ {application, sumo_rest}
  , {dialyzer_warnings, DWs}
  , {dirs, [ "ebin"
           %% , "test"
           %%
           %% Do not include `"test"` in order to avoid spurious
           %% warnings with `unknown` e.g. [`Unknown function
           %% ct:comment/1`](https://github.com/inaka/katana-test/issues/38).
           , "test/sr_test"
           ]}
  | Config
  ].
