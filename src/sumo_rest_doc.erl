%%% @doc Implement this behavior on your entities so the handlers can
%%% properly (un)marshall them.
-module(sumo_rest_doc).

-type key() :: binary() | atom().
-type object() :: #{key() => json()}.
-type json() :: object()
              | [object()]
              | binary()
              | number()
              | boolean()
              | null.

-export_type([json/0]).

-type entity() :: sumo:user_doc().
-export_type([entity/0]).

-type reason() :: iodata().
-export_type([reason/0]).

-callback to_json(entity()) -> json().
-callback from_json(json()) -> {ok, entity()} | {error, reason()}.
-callback uri_path(entity()) -> iodata().
%% @todo optional callback (it's only needed if dups should raise 409 conflict)
-callback id(entity()) -> term().
