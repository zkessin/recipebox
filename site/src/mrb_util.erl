-module(mrb_util).
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-compile(export_all).
-endif.
%-export([list_recipes_by_user/1, add_recipe/1, get_recipe_by_id/1]).
-type(id() :: binary()).
-record(recipe, {

          id          :: id(), %% GUID
          owner       :: binary(),
          name        :: string(),
          ingredients :: string(),
          directions  :: string()}).

setup_mnesia_tables() ->
    case mnesia:create_table(recipe, [ {attributes, record_info(fields,recipe)}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _}} ->
            ok;
        R ->
            ?debugVal(R),
            ?assert(false)
    end.

-spec(add_recipe(#recipe{}|[#recipe{}]) -> ok).
add_recipe(Recipe = #recipe{}) ->
    F = fun() ->
                mnesia:write(Recipe)
        end,
    {atomic,ok} = mnesia:transaction(F),
    ok;
add_recipe(Recipes) when is_list(Recipes)->
    F = fun() ->
                [mnesia:write(Recipe) || Recipe<-Recipes]
        end,
    {atomic,_} = mnesia:transaction(F),
    ok.

-spec(get_recipe_by_id(id()) ->                
             {ok,#recipe{}}| not_found).
get_recipe_by_id(ID) ->
    [Recipe] = mnesia:dirty_match_object(recipe,#recipe{id=ID, _='_'}),
    {ok, Recipe}.

list_recipes_by_user(UserId) ->
    mnesia:dirty_match_object(recipe, #recipe{owner= UserId, _='_'}).
