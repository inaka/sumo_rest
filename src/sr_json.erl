%%% @doc Json abstraction library
-module(sr_json).

-export([encode/1, decode/1]).
-export([encode_date/1, decode_date/1]).

-type key() :: binary() | atom().
-type object() :: #{key() => json()}.
-type json() :: object()
              | [object()]
              | binary()
              | number()
              | boolean()
              | null.

-export_type([json/0]).

-spec encode(json()) -> iodata().
encode(Json) ->
  try jiffy:encode(Json, [uescape])
  catch
    _:_Error ->
      throw(notjson)
  end.

-spec decode(iodata()) -> json().
decode(Data) ->
  try jiffy:decode(Data, [return_maps])
  catch
    _:_Error ->
      throw(badjson)
  end.

-spec encode_date(calendar:datetime()) -> binary().
encode_date(DateTime) -> iso8601:format(DateTime).

%% @todo remove binary_to_list when is8601 specs are fixed
-spec decode_date(binary()) -> calendar:datetime().
decode_date(DateTime) -> iso8601:parse(binary_to_list(DateTime)).
