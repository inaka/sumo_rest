# Sumo Rest

<img src="http://www.technovelgy.com/graphics/content/sumo_robot.jpg" align="right" style="float:right" height="400" />

Generic cowboy handlers to work with Sumo D

## Introduction
We, at Inaka, are used to build our RESTful servers on top of [cowboy](https://github.com/ninenines/cowboy). We use [sumo_db](https://github.com/inaka/sumo_db) to manage our persistence and [trails](https://github.com/inaka/cowboy-trails) together with [cowboy-swagger](https://github.com/inaka/cowboy-swagger) for documentation.

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
    + `GET /entity/:id` - to retrieve an entity
    + `PUT /entity/:id` - to update (or create) an entity
    + `PATCH /entity/:id` - to update an entity
    + `DELETE /entity/:id` - to delete an entity

(Of course, the uris for those endpoints will not be exactly those, you have to define what _entities_ you want to manage.)

To use them you first have to define your models, by implementing the behaviours `sumo_doc` (from **Sumo DB**) and [`sumo_rest_doc`](src/sumo_rest_doc.erl).

Then you have to create a module that implements the `trails_handler` behaviour (from **Trails**) and _mix in_ that module all the functions that you need from the provided handlers.

## A Basic Example
You can find a very basic example of the usage of this app in the [tests](tests/sr_test).
Let me walk you through the process of creating such a simple app.

### The application definition
In [sr_test.app](tests/sr_test.app) file you'll find the usual stuff. The only particular pieces are:

* The list of `applications`, which includes `cowboy`, `katana`, `cowboy_swagger` and `sumo_db`.
* The list of `start_phases`. This is not a requirement, but we've found this is a nice way of getting **Sumo DB** up and running before **Cowboy** starts listening:
```erlang
  { start_phases
  , [ {create_schema, []}
    , {start_cowboy_listeners, []}
    ]
  }
```


## A Full-Fledged App
For a more elaborated example on how to use this library, please check [lsl](https://github.com/inaka/lsl).

---

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).
