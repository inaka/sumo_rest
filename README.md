# Sumo Rest

<img src="http://www.technovelgy.com/graphics/content/sumo_robot.jpg" align="right" style="float:right" height="400" />

Generic cowboy handlers to work with Sumo DB

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

<img src="https://docs.google.com/drawings/d/1mlJTIxd7mH_48hcWmip_zW6rfzglbmSprpGSsfhjcsM/pub?w=367&amp;h=288" style="float:left" height="288" />

As you'll see below, **Sumo Rest** gives you _base handlers_ that you can use on your **Cowboy** server to manage your **Sumo DB** entities easily. You just need to define your routes using **Trails** and provide proper metadata for each of them. In particular, you need to provide the same basic metadata **Swagger** requires. You can manually use the base handlers and call each of their functions when you need them, but you can also use **Mixer** to just _bring_ their functions to your own handlers easily.

## Example App
For an example on how to use this library, please check [lsl](https://github.com/inaka/lsl)

---

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).
