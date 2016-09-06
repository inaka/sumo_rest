%%% @doc POST /sessions handler
-module(sr_sessions_handler).

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
        , is_authorized/2
        , handle_post/2
        ]).

-type state() :: sr_entities_handler:state().

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
       #{ tags => ["sessions"]
        , description => "Creates a new session"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/sessions",
  Opts = #{ path => Path
          , model => sr_sessions
          , verbose => true
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  case get_authorization(Req) of
    {not_authenticated, Req1} ->
      {{false, auth_header()}, Req1, State};
    {User, Req1} ->
      Users = application:get_env(sr_test, users, []),
      case lists:member(User, Users) of
        true -> {true, Req1, State#{user => User}};
        false ->
          ct:log("Invalid user ~p not in ~p", [User, Users]),
          {{false, auth_header()}, Req1, State}
      end
  end.

-spec get_authorization(cowboy_req:req()) ->
    {{binary(), binary()}, cowboy_req:req()}
  | {not_authenticated, cowboy_req:req()}.
get_authorization(Req) ->
  try cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Key, Secret}}, Req1} ->
      {{Key, Secret}, Req1};
    {ok, Value, Req1} ->
      WarnMsg = "Invalid basic authentication: ~p~n",
      error_logger:warning_msg(WarnMsg, [Value]),
      {not_authenticated, Req1};
    {error, badarg} ->
      {Hdr, Req1} = cowboy_req:header(<<"authorization">>, Req),
      WarnMsg = "Malformed authorization header: ~p~n",
      error_logger:warning_msg(WarnMsg, [Hdr]),
      {not_authenticated, Req1}
  catch
    _:Error ->
      WarnMsg = "Error trying to parse auth: ~p~nStack: ~s",
      error_logger:warning_msg(WarnMsg, [Error, erlang:get_stacktrace()]),
      {not_authenticated, Req}
  end.

-spec auth_header() -> binary().
auth_header() -> <<"Basic Realm=\"Sumo Rest Test\"">>.

-spec handle_post(cowboy_req:req(), map()) ->
  {{true, binary()} | false | halt, cowboy_req:req(), state()}.
handle_post(Req, #{opts := Opts} = State) ->
  #{user := {User, _}} = State,
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Json             = sr_json:decode(Body),
    case sr_sessions:from_json(Json) of
      {error, Reason} ->
        Req2 = cowboy_req:set_resp_body(sr_json:error(Reason), Req1),
        {false, Req2, State};
      {ok, Session} ->
        FullSession = sr_sessions:user(Session, User),
        State2 = #{opts => Opts},
        sr_entities_handler:handle_post(FullSession, Req1, State2)
    end
  catch
    _:conflict ->
      {ok, Req3} =
        cowboy_req:reply(409, [], sr_json:error(<<"Duplicated entity">>), Req),
      {halt, Req3, State};
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.
