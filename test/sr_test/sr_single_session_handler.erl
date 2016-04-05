%%% @doc GET|DELETE /sessions/:id handler
-module(sr_single_session_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_put/2
          , delete_resource/2
          ]
        }]).
-mixin([{ sr_sessions_handler
        , [ is_authorized/2
          ]
        }]).

-export([ trails/0
        , forbidden/2
        , is_conflict/2
        ]).

-type state() :: sr_single_entity_handler:state().

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
     , description => <<"Session Id">>
     , required => true
     , type => string
     },
  Metadata =
    #{ put =>
       #{ tags => ["sessions"]
        , description => "Updates a session"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody, Id]
        }
     , delete =>
       #{ tags => ["sessions"]
        , description => "Deletes a session"
        , parameters => [Id]
        }
     },
  Path = "/sessions/:id",
  Opts = #{ path => Path
          , model => sr_sessions
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec forbidden(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
forbidden(Req, State) ->
  Id = cowboy_req:binding(id, Req),
  #{user := {User, _}} = State,
  %% #{user := {User, _}, id := Id} = State,
  case sumo:find(sr_sessions, Id) of
    notfound -> {false, Req, State};
    Session -> {User =/= sr_sessions:user(Session), Req, State}
  end.

-spec is_conflict(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
is_conflict(Req, State) ->
  {not maps:is_key(entity, State), Req, State}.
