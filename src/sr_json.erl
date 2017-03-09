%%% @doc Json abstraction library
-module(sr_json).

-export([encode/1, decode/1]).
-export([encode_date/1, decode_date/1]).
-export([encode_null/1, decode_null/1]).
-export([error/1]).

-type key() :: binary() | atom().
-type object() :: #{key() => json()}.
-type json() :: object()
              | [json()]
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

%% @doc Internal representation to string
-spec encode(json()) -> iodata().
encode(Json) -> jsx:encode(Json, [uescape]).

%% @doc String to internal representation
-spec decode(iodata()) -> json().
decode(Data) ->
  try jsx:decode(Data, [return_maps])
  catch
    _:_Error ->
      throw(badjson)
  end.

%% @doc Format datetimes as binaries using iso8601
-spec encode_date(calendar:datetime()) -> binary().
encode_date(DateTime) -> iso8601:format(DateTime).

%% @doc Parse binaries as datetimes using iso8601
%% @todo remove binary_to_list when is8601 specs are fixed
-spec decode_date(binary()) -> calendar:datetime().
decode_date(DateTime) -> iso8601:parse(binary_to_list(DateTime)).

%% @doc Encode 'undefined' as 'null'.
%%      Leave everything else as is.
-spec encode_null(undefined) -> null
               ; (json()) -> json().
encode_null(undefined) -> null;
encode_null(Json) -> Json.

%% @doc Decode 'null' as 'undefined'.
%%      Leave everything else as is.
-spec decode_null(null) -> undefined
               ; (non_null_json()) -> json().
decode_null(null) -> undefined;
decode_null(Json) -> Json.

%% @doc Format errors as jsons.
%%      Given the error Reason, this function returns the json equivalent to
%%      <code>{"error": "Reason"}</code>.
-spec error(binary()) -> iodata().
error(Error) -> encode(#{error => Error}).
