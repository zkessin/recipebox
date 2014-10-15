%% Include the automatically generated plugins directory
-include("plugins.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%% Include any application-specific custom elements, actions, or validators below
%% basic types (with tags for pattern matching)
-type(maybe(X)         :: {ok, X}| not_found). 
-type(maybe_list(X)    :: X|[X]).
-type(error(X)         :: {ok, X} | {error, term()}).
-type(match(X)         :: X|'_').
-type(user_id()        :: {user_id,      binary()}).
-type(recipe_id()      :: {recipe_id,    binary()}).
-type(recipe_name()    :: {recipe_name,  binary()}).
-type(email()          :: {email,        binary()}).
-type(first_name()     :: {first_name,   binary()}).
-type(full_name()      :: {full_name,    binary()}).
-type(access_token()   :: {access_token, binary()}).
-type(timestamp()      :: {timestamp,    term()  }).
-type(ingredients()    :: {ingredients,  binary()}).
-type(directions()     :: {directions,   binary()}).


-record(recipe, {
          id           :: match(recipe_id()), %% GUID
          owner        :: match(user_id()),
          name         :: match(recipe_name()),
          ingredients  :: match(ingredients()),
          directions   :: match(directions()),
          timestamp    :: match(timestamp())}).

-record(user, {
          user_id      :: match(user_id()),
          email        :: match(email()),
          full_name    :: match(full_name()),
          first_name   :: match(first_name()),
          access_token :: match(access_token()),
          timestamp    :: match(timestamp())
          }).
-type(user_rec() ::#user{}).

%--------------------------------------------------------------------------------
% Macros
-define(GET(Field),
        {Field, list_to_binary(wf:q(Field))}).

-define(GET_JSON(JSON_TERM),
        fun(Field) when is_atom(Field) ->
                FieldB = erlang:atom_to_binary(Field,utf8),
                Val    = proplists:get_value(FieldB, JSON_TERM),
                {Field, Val}
        end).
