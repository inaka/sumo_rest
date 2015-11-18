%%% @doc Json abstraction library
-module(sr_json).

-export([encode/1, decode/1]).
-export([encode_date/1, decode_date/1, encode_null/1]).

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
encode_date({{Year, Month, Day}, {Hour, Min, Sec}}) ->
  iolist_to_binary(io_lib:format(
    "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0wZ",
    [Year, Month, Day, Hour, Min, Sec])).

-spec decode_date(binary()) -> calendar:datetime().
decode_date(DateTime) ->
  [Date, Time] = binary:split(DateTime, <<"T">>),
  [Year, Month, Day] = binary:split(Date, <<"-">>, [global]),
  [Hour, Min, Sec] = binary:split(Time, <<":">>, [global]),
  { { decode_number(Year)
    , decode_number(Month)
    , decode_number(Day)
    }
  , { decode_number(Hour)
    , decode_number(Min)
    , decode_number(Sec)
    }
  }.

decode_number(Bin) ->
  {X, _} = string:to_integer(binary_to_list(Bin)),
  X.

-spec encode_null(undefined | json()) -> json().
encode_null(undefined) -> null;
encode_null(Json) -> Json.
