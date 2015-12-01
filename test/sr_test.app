{application, sr_test, [
  {description, "Test app for sumo_rest"},
  {id, "sr_test"},
  {applications,
    [ kernel
    , stdlib
    , crypto
    , sasl

    , cowboy
    , trails
    , cowboy_swagger
    , jiffy
    , katana
    , sumo_db
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
