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

-callback to_json(sumo:user_doc()) -> json().
-callback from_json(json()) -> sumo:user_doc().
