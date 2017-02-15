%%% @doc Implement this behavior on your entities so the handlers can
%%% properly [un]marshall them.
-module(sumo_rest_doc).

-type json() :: sr_json:json().

-type entity() :: sumo:user_doc().
-export_type([entity/0]).

-type path() :: string().
-export_type([path/0]).

-type reason() :: iodata().
-export_type([reason/0]).

-type duplication_conditions() :: sumo:conditions().
-export_type([duplication_conditions/0]).

-callback to_json(entity()) -> json().
-callback from_json(json()) -> {ok, entity()} | {error, reason()}.
-callback update(entity(), json()) -> {ok, entity()} | {error, reason()}.
-callback location(entity(), path()) -> iodata().
%% it's only needed if dups should raise 422 conflict
-callback duplication_conditions(entity()) -> duplication_conditions().
%% it's only needed if ids are not coming in PUT jsons
-callback from_json(binary(), json()) -> {ok, entity()} | {error, reason()}.
%% it's only needed if ids are different from binary, integer or string
-callback id_from_binding(binary()) -> term().

-optional_callbacks([duplication_conditions/1, from_json/2, id_from_binding/1]).
