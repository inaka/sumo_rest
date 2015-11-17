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

init_per_suite(Config) -> [{application, sumo_rest} | Config].
