%%% @doc /echo handler
-module(sr_echo_request_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_put/2
          ]
        }]).

-export([ trails/0
        ]).

-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Metadata =
    #{ put =>
       #{ tags => ["echo"]
        , description => "save an echo request"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/echo/:id",
  Opts = #{ path => Path
          , model => echo_request
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
