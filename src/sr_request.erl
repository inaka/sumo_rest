-module(sr_request).

-export([ from_cowboy/1
        , body/1
        , headers/1
        , path/1
        , bindings/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-opaque req() ::
  #{ body     := sr_json:json()
   , headers  := [{binary(), iodata()}]
   , path     := binary()
   , bindings := #{atom() => any()}
   }.

-export_type [req/0].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constructor, getters/setters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec from_cowboy(cowboy_req:req()) -> {req(), cowboy_req:req()}.
from_cowboy(CowboyReq) ->
  {ok, RawBody, CowboyReq1} = cowboy_req:body(CowboyReq),
  Body = sr_json:decode(RawBody),
  {Headers, CowboyReq2} = cowboy_req:headers(CowboyReq1),
  {Path, CowboyReq3} = cowboy_req:path(CowboyReq2),
  {BindingsList, CowboyReq4} = cowboy_req:bindings(CowboyReq3),
  Request = #{ body     => Body
             , headers  => Headers
             , path     => Path
             , bindings => maps:from_list(BindingsList)
             },
  {Request, CowboyReq4}.

-spec body(req()) -> sr_json:json() | undefined.
body(#{body := Body}) ->
  Body.

-spec headers(req()) -> [{binary(), iodata()}].
headers(#{headers := Headers}) ->
  Headers.

-spec path(req()) -> binary().
path(#{path := Path}) ->
  Path.

-spec bindings(req()) -> #{atom() => any()}.
bindings(#{bindings := Bindings}) ->
  Bindings.
