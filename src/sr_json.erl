%%% @doc Json abstraction library
-module(sr_json).

-export([encode/1, decode/1]).
-export([encode_date/1, decode_date/1]).
-export([encode_null/1, decode_null/1]).
-export([error/1]).

-type key() :: binary() | atom().
-type object() :: #{key() => json()}.
-type json() :: object()
              | [object()]
              | binary()
              | number()
              | boolean()
              | null
              .
-type non_null_json() :: object()
                       | [object()]
                       | binary()
                       | number()
                       | boolean()
                       .

-export_type([json/0]).

-spec encode(json()) -> iodata().
encode(Json) -> jiffy:encode(Json, [uescape]).

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

-spec encode_null(undefined) -> null
               ; (json()) -> json().
encode_null(undefined) -> null;
encode_null(Json) -> Json.

-spec decode_null(null) -> undefined
               ; (non_null_json()) -> json().
decode_null(null) -> undefined;
decode_null(Json) -> Json.

-spec error(binary()) -> iodata().
error(Error) -> encode(#{error => Error}).
