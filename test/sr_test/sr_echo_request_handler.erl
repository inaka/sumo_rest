%%% @doc POST|GET /echo handler
-module(sr_echo_request_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          ]
        }]).

-export([ trails/0
        , handle_post/2
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
    #{ post =>
       #{ tags => ["echo"]
        , description => "Returns the list of echo"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/echo",
  Opts = #{ path => Path
          , model => echo_request
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec handle_post(cowboy_req:req(), sr_state:state()) ->
  {{true, binary()}, cowboy_req:req(), sr_state:state()}.
handle_post(Req, State) ->
  Module = sr_state:module(State),
  {SrRequest, Req1} = sr_request:from_cowboy(Req),
  Context = #{req => SrRequest, state => State},
  {ok, Entity} = apply(Module, from_ctx, [Context]),
  JSON = sr_json:encode(apply(Module, to_json, [Entity])),
  {ok, Req2} = cowboy_req:reply(201, [], JSON, Req1),
  {{true, <<"/echo">>}, Req2, State}.
