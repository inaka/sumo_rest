%%% @doc POST|GET /entities handler
-module(sr_entities_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_base_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , is_authorized/2
          , resource_exists/2
          , content_types_provided/2
          , handle_get/2
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
    #{ get =>
       #{ tags => ["entities"]
        , description => "Returns the list of entities"
        , produces => ["application/json"]
        }
     , post =>
       #{ tags => ["entities"]
        , description => "Creates a new entity"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/entities",
  Opts = #{ path => Path
          , model => sr_entities
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
