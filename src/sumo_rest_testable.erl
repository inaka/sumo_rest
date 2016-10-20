-module(sumo_rest_testable).

-type request() :: {binary(), binary()
                            | number()
                            | boolean()
                            | null}.
-export_type([request/0]).

-callback get_examples() -> [request()].
-callback post_examples() -> [request()].

-optional_callbacks([get_examples/0, post_examples/0]).