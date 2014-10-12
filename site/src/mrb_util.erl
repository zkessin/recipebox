-module(mrb_util).

-ifdef(TEST).
-compile(export_all).
-endif.
%-export([list_recipes_by_user/1, add_recipe/1, get_recipe_by_id/1]).

-record(recipe, {
          owner       :: binary(),
          id          :: binary(), %% GUID
          name        :: string(),
          ingredients :: string(),
          directions  :: string()}).

