{application, sr_test, [
  {description, "Test app for sumo_rest"},
  {id, "sr_test"},
  {applications,
    [ kernel
    , stdlib
    , crypto
    , sasl
    , cowboy
    , cowboy_swagger
    , jsx
    , sumo_db
    , mnesia
    ]},
  {modules, []},
  {mod, {sr_test, []}},
  {registered, []},
  { start_phases
  , [ {create_schema, []}
    , {start_cowboy_listeners, []}
    ]
  }
  ]}.
