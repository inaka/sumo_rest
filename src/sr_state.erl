-module(sr_state).

%% constructor, getters and setters
-export([ new/2
        , id/1
        , id/2
        , entity/1
        , entity/2
        , module/1
        , opts/1
        ]).

%% functions to work with user opts
-export([ set/3
        , retrieve/3
        , remove/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type options() ::
  #{ path := string()
   , model := atom()
   , verbose => boolean()
   }.

-opaque state() ::
  #{ opts      := options()
   , id        => binary()
   , entity    => sumo:user_doc()
   , module    := module()
   , user_opts := map()
   }.

-export_type([state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constructor, Getters/Setters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(options(), module()) -> state().
new(Opts, Module) ->
  #{opts => Opts, module => Module, user_opts => #{}}.

-spec id(state()) -> binary() | undefined.
id(#{id := Id}) ->
  Id;
id(_State) ->
  undefined.

-spec id(state(), binary()) -> state().
id(State, Id) ->
  State#{id => Id}.

-spec entity(state()) -> sumo:user_doc() | undefined.
entity(#{entity := Entity}) ->
  Entity;
entity(_State) ->
  undefined.

-spec entity(state(), sumo:user_doc()) -> state().
entity(State, Entity) ->
  State#{entity => Entity}.

-spec module(state()) -> module().
module(#{module := Module}) ->
  Module.

-spec opts(state()) -> options().
opts(#{opts := Opts}) ->
  Opts.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% User opts Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec set(Key :: term(), Value :: term(), state()) -> state().
set(Key, Value, #{user_opts := UserOpts} = State) ->
  NewUserOpts = UserOpts#{Key => Value},
  State#{user_opts => NewUserOpts}.

-spec retrieve(Key :: term(), state(), Default :: term()) -> term().
retrieve(Key, #{user_opts := UserOpts}, Default) ->
  maps:get(Key, UserOpts, Default).

-spec remove(Key :: term(), state()) -> state().
remove(Key, #{user_opts := UserOpts} = State) ->
  NewUserOpts = maps:remove(Key, UserOpts),
  State#{user_opts => NewUserOpts}.
