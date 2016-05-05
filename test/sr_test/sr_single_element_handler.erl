%%% @doc GET|PUT|DELETE /elements/:id handler
-module(sr_single_element_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_put/2
          , handle_patch/2
          , delete_resource/2
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
  Id =
    #{ name => id
     , in => path
     , description => <<"Element Key">>
     , required => true
     , type => string
     },
  Metadata =
    #{ get =>
       #{ tags => ["elements"]
        , description => "Returns an element"
        , produces => ["application/json"]
        , parameters => [Id]
        }
     , patch =>
       #{ tags => ["elements"]
        , description => "Updates an element"
        , consumes => ["application/json", "application/json; charset=utf-8"]
        , produces => ["application/json"]
        , parameters => [RequestBody, Id]
        }
     , put =>
       #{ tags => ["elements"]
        , description => "Updates or creates a new element"
        , consumes => ["application/json", "application/json; charset=utf-8"]
        , produces => ["application/json"]
        , parameters => [RequestBody, Id]
        }
     , delete =>
       #{ tags => ["elements"]
        , description => "Deletes an element"
        , parameters => [Id]
        }
     },
  Path = "/elements/:id",
  Opts = #{ path => Path
          , model => sr_elements
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
