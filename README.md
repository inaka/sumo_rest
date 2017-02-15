# Sumo Rest

<img src="http://www.technovelgy.com/graphics/content/sumo_robot.jpg" align="right" style="float:right" height="400" />

Generic **Cowboy** handlers to work with **Sumo DB**

## Introduction
We, at Inaka, build our RESTful servers on top of [cowboy](https://github.com/ninenines/cowboy). We use [sumo_db](https://github.com/inaka/sumo_db) to manage our persistence and [trails](https://github.com/inaka/cowboy-trails) together with [cowboy-swagger](https://github.com/inaka/cowboy-swagger) for documentation.

Soon enough, we realized that we were duplicating code everywhere. Not every endpoint in our APIs is just a CRUD for some entity, but there are definitely lots of them in every server. As an example, most of our servers provide something like the following list of endpoints:

* `GET /users` - Returns the list of users
* `POST /users` - Creates a new user
* `PUT /users/:id` or `PATCH /users/:id` - Updates a user
* `DELETE /users/:id` - Deletes a user
* `GET /users/:id` - Retrieves an individual user

To avoid (or at least reduce) such duplication, we started using [mixer](https://github.com/chef/mixer). That way, we can have a *base_handler* in each application where all the common handler logic lives.

Eventually, all applications shared that same *base_handler*, so we decided to abstract that even further. Into its own app: **sumo_rest**.

## Architecture
This project dependency tree is a great way to show the architecture behind it.

![Architecture](https://docs.google.com/drawings/d/1mlJTIxd7mH_48hcWmip_zW6rfzglbmSprpGSsfhjcsM/pub?w=367&amp;h=288)

As you'll see below, **Sumo Rest** gives you _base handlers_ that you can use on your **Cowboy** server to manage your **Sumo DB** entities easily. You just need to define your routes using **Trails** and provide proper metadata for each of them. In particular, you need to provide the same basic metadata **Swagger** requires. You can manually use the base handlers and call each of their functions when you need them, but you can also use **Mixer** to just _bring_ their functions to your own handlers easily.

## Usage
In a nutshell, **Sumo Rest** provides 2 cowboy rest handlers:

- [`sr_entities_handler`](src/sr_entities_handler.erl) that provides an implementation for
    + `POST /entities` - to create a new entity
    + `GET /entitites` - to retrieve the list of all entities
- [`sr_single_entity_handler`](src/sr_single_entity_handler.erl) that provides implementation for
    + `GET /entities/:id` - to retrieve an entity
    + `PUT /entities/:id` - to update (or create) an entity
    + `PATCH /entities/:id` - to update an entity
    + `DELETE /entities/:id` - to delete an entity

(Of course, the uris for those endpoints will not be exactly those, you have to define what _entities_ you want to manage.)

To use them you first have to define your models, by implementing the behaviours `sumo_doc` (from **Sumo DB**) and [`sumo_rest_doc`](src/sumo_rest_doc.erl).

Then you have to create a module that implements the `trails_handler` behaviour (from **Trails**) and _mix in_ that module all the functions that you need from the provided handlers.

## A Basic Example
You can find a very basic example of the usage of this app in the [tests](test/sr_test).

The app used for the tests (`sr_test`), makes no sense at all. Don't worry about that. It's just there to provide examples of usage (and of course to run the tests). It basically manages 2 totally independent entities:
- _elements_: members of an extremely naïve key/value store
- _sessions_: poorly-designed user sessions :trollface:

Let me walk you through the process of creating such a simple app.

### The application definition
In [sr_test.app](test/sr_test.app) file you'll find the usual stuff. The only particular pieces are:

* The list of `applications`, which includes `cowboy`, `katana`, `cowboy_swagger` and `sumo_db`.
* The list of `start_phases`. This is not a requirement, but we've found this is a nice way of getting **Sumo DB** up and running before **Cowboy** starts listening:
```erlang
  { start_phases
  , [ {create_schema, []}
    , {start_cowboy_listeners, []}
    ]
  }
```

### The configuration
In [test.config](test/test.config) we added the required configuration for the different apps to work:

#### Swagger
We just defined the minimum required properties:
```erlang
, { cowboy_swagger
  , [ { global_spec
      , #{ swagger => "2.0"
         , info => #{title => "SumoRest Test API"}
         , basePath => ""
         }
      }
    ]
  }
```

#### Mnesia
We've chosen **Mnesia** as our backend, so we just enabled debug on it (not a requirement, but a nice thing to have on development environments):
```erlang
, { mnesia
  , [{debug, true}]
  }
```

#### Sumo DB
**Sumo DB**'s **Mnesia** backend/store is really easy to set up. We will just have 2 models: _elements_ and _sessions_. We will store them both on **Mnesia**:
```erlang
, { sumo_db
  , [ {wpool_opts, [{overrun_warning, 100}]}
    , {log_queries, true}
    , {query_timeout, 30000}
    , {storage_backends, []}
    , {stores, [{sr_store_mnesia, sumo_store_mnesia, [{workers, 10}]}]}
    , { docs
      , [ {elements, sr_store_mnesia, #{module => sr_elements}}
        , {sessions, sr_store_mnesia, #{module => sr_sessions}}
        ]
      }
    , {events, []}
    ]
  }
```

#### SR Test
Finally we add some extremely naïve configuration to our own app. In our case, just a list of users we'll use for authentication purposes (:warning: **Do NOT do this at home, kids** :warning:):
```erlang
, { sr_test
  , [ {users, [{<<"user1">>, <<"pwd1">>}, {<<"user2">>, <<"pwd2">>}]}
    ]
  }
```

### The application module
The next step is to come up with the main application module: [sr_test](test/sr_test/sr_test.erl). The interesting bits are all in the start phases.

#### `create_schema`
For **Sumo DB** to work, we just need to make sure we create the schema. We need to do a little trick to setup **Mnesia** though, because for `create_schema` to properly work, **Mnesia** has to be stopped:
```erlang
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema();
```

#### `start_cowboy_listeners`
Since we're using **Trails**, we can let each module define its own ~~routes~~ trails. And, since we're using a single host we can use the fancy helper that comes with **Trails**:
```erlang
  Handlers =
    [ sr_elements_handler
    , sr_single_element_handler
    , sr_sessions_handler
    , sr_single_session_handler
    , cowboy_swagger_handler
    ],
  Routes = trails:trails(Handlers),
  trails:store(Routes),
  Dispatch = trails:single_host_compile(Routes),
```
It's crucial that we _store_ the trails. Otherwise, **Sumo Rest** will not be able to find them later.

Then, we start our **Cowboy** server:
```erlang
  TransOpts = [{port, 4891}],
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  case cowboy:start_http(sr_test_server, 1, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end.
```

### The Models
The next step is to define our models (i.e. the entities our system will manage). We use a module for each model and all of them implement the required behaviours.

#### Elements
[Elements](test/sr_test/sr_elements.erl) are simple key/value pairs.
```erlang
-type key() :: integer().
-type value() :: binary() | iodata().

-opaque element() ::
  #{ key        => key()
   , value      => value()
   , created_at => calendar:datetime()
   , updated_at => calendar:datetime()
   }.
```

`sumo_doc` requires us to add the schema, sleep and wakeup functions. Since we'll use maps for our internal representation (just like **Sumo DB** does), they're trivial:
```erlang
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(elements,
    [ sumo:new_field(key,        string,   [id, not_null])
    , sumo:new_field(value,      string,   [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    , sumo:new_field(updated_at, datetime, [not_null])
    ]).

-spec sumo_sleep(element()) -> sumo:doc().
sumo_sleep(Element) -> Element.

-spec sumo_wakeup(sumo:doc()) -> element().
sumo_wakeup(Element) -> Element.
```

`sumo_rest_doc` on the other hand requires functions to convert to and from json (which should also validate user input):
```erlang
-spec to_json(element()) -> sumo_rest_doc:json().
to_json(Element) ->
  #{ key        => maps:get(key, Element)
   , value      => maps:get(value, Element)
   , created_at => sr_json:encode_date(maps:get(created_at, Element))
   , updated_at => sr_json:encode_date(maps:get(updated_at, Element))
   }.

-spec from_json(sumo_rest_doc:json()) -> {ok, element()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ key        => maps:get(<<"key">>, Json)
       , value      => maps:get(<<"value">>, Json)
       , created_at =>
          sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , updated_at =>
          sr_json:decode_date(maps:get(<<"updated_at">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.
```

We also need to provide an `update` function for `PUT` and `PATCH`:
```erlang
-spec update(element(), sumo_rest_doc:json()) ->
  {ok, element()} | {error, iodata()}.
update(Element, Json) ->
  try
    NewValue = maps:get(<<"value">>, Json),
    UpdatedElement =
      Element#{value := NewValue, updated_at := calendar:universal_time()},
    {ok, UpdatedElement}
  catch
    _:{badkey, Key} ->
      {error, <<"missing field: ", Key/binary>>}
  end.
```

For **Sumo Rest** to provide urls to the callers, we need to specify the location URL:
```erlang
-spec location(element(), sumo_rest_doc:path()) -> binary().
location(Element, Path) -> iolist_to_binary([Path, "/", key(Element)]).
```

To let **Sumo Rest** avoid duplicate keys (and return `422 Conflict` in that case), we provide the optional callback `duplication_conditions/1`:
```erlang
-spec duplication_conditions(element()) -> sumo_rest_doc:duplication_conditions().
duplication_conditions(Element) -> [{key, '==', key(Element)}].
```

If your model has an `id` type different than integer, string or binary you have to implement `id_from_binding/1`. That function is needed in order to convert the `id` from `binary()` to your type. There is an example at `sr_elements` module for our test coverage. It only converts to `integer()` but that is the general idea behind that function.
```erlang
-spec id_from_binding(binary()) -> key().
id_from_binding(BinaryId) ->
  try binary_to_integer(BinaryId) of
    Id -> Id
  catch
    error:badarg -> -1
  end.
```

The rest of the functions in the module are just helpers, particularly useful for our tests.

#### Sessions
[Sessions](test/sr_test/sr_sessions.erl) are very similar to elements. The only difference here is that session ids (unlike element keys) are auto-generated by the mnesia store. Therefore they're initially `undefined`. We don't need to provide a `duplication_conditions/1` function in this case since we don't need to avoid duplicates.

### The Handlers
Now, the juicy part: The cowboy handlers. We have 4, two of them built on top of `sr_entitites_handler` and the other two built on `sr_single_entity_handler`.

#### Elements
[sr_elements_handler](test/sr_test/sr_elements_handler.erl) is built on `sr_entities_handler` and handles the path `"/elements"`. As you can see, the code is really simple.

First we _mix in_ the functions from `sr_entities_handler`:
```erlang
-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_post/2
          ]
        }]).
```

Then, we only need to write the documentation for this module, and provide the proper `Opts` and that's all:
```erlang
-spec trails() -> trails:trails().
trails() ->
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Metadata =
    #{ get =>
       #{ tags => ["elements"]
        , description => "Returns the list of elements"
        , produces => ["application/json"]
        }
     , post =>
       #{ tags => ["elements"]
        , description => "Creates a new element"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [RequestBody]
        }
     },
  Path = "/elements",
  Opts = #{ path => Path
          , model => elements
          },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].
```
The `Opts` here include the trails path (so it can be found later) and the model behind it.

And there you go, **_no more code!_**

[`sr_single_element_handler`](test/sr_test/sr_single_element_handler.erl) is analogous but it's based on `sr_single_entity_handler`.

#### Sessions
[sr_sessions_handler](test/sr_test/sr_sessions_handler.erl) shows you what happens when you need to steer away from the default implementations in **Sumo Rest**. It's as easy as defining your own functions instead of _mixing_ them _in_ from the base handlers.

In this case we needed authentication, so we added an implementation for `is_authorized`:
```erlang
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
          ct:pal("Invalid user ~p not in ~p", [User, Users]),
          {{false, auth_header()}, Req1, State}
      end
  end.
```

And then we redefined `handle_post/2` to use the authenticated user there:
```erlang
-spec handle_post(cowboy_req:req(), state()) ->
  {{true, binary()}, cowboy_req:req(), state()}.
handle_post(Req, State) ->
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
        sr_entities_handler:handle_post(FullSession, Req1, State)
    end
  catch
    _:conflict ->
      {ok, Req3} =
        cowboy_req:reply(422, [], sr_json:error(<<"Duplicated entity">>), Req),
      {halt, Req3, State};
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.
```

As you can see we still use `sr_entities_handler:handle_post/3` there, once we're past the parsing stage.

Finally, we did something similar in [`sr_single_session_handler`](test/sr_test/sr_single_session_handler.erl). We needed the same authentication mechanism, so we just _mix_ it _in_:
```erlang
-mixin([{ sr_sessions_handler
        , [ is_authorized/2
          ]
        }]).
```

But we needed to prevent users from accessing other user's sessions, so we implemented `forbidden/2`:
```erlang
-spec forbidden(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
forbidden(Req, State) ->
  #{user := {User, _}, id := Id} = State,
  case sumo:find(sessions, Id) of
    notfound -> {false, Req, State};
    Session -> {User =/= sr_sessions:user(Session), Req, State}
  end.
```

And, since sessions can not be created with `PUT` (because their keys are auto-generated):
```erlang
-spec is_conflict(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
is_conflict(Req, State) ->
  {not maps:is_key(entity, State), Req, State}.
```

## A Full-Fledged App
For a more elaborated example on how to use this library, please check [lsl](https://github.com/inaka/lsl).

---

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/sumo_rest/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).
