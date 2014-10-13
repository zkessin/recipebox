-module(mrb_util).
-include_lib("eunit/include/eunit.hrl").

-include_lib("nitrogen_core/include/wf.hrl").
-ifdef(TEST).
-compile(export_all).
-endif.
-export([list_recipes_by_user/1, add_recipe/1, get_recipe_by_id/1, format_recipe/1]).
-type(recipe_id() :: binary()).
-record(recipe, {

          id          :: recipe_id(), %% GUID
          owner       :: binary(),
          name        :: string(),
          ingredients :: string(),
          directions  :: string()}).

make_uurecipe_id() ->
    V4 = uuid:get_v4(),
    StrV4 = uuid:uuid_to_string(V4),
    list_to_binary(StrV4).

make_recipe(Owner, Name, Ingredients, Directions) ->
    {ok, #recipe{ 
            id          = make_uurecipe_id(),
            owner       = list_to_binary(Owner),
            name        = list_to_binary(Name),
            ingredients = list_to_binary(Ingredients),
            directions  = list_to_binary(Directions)}}.
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

-spec(format_recipe(#recipe{}) -> nitrogen_element()).
format_recipe(#recipe{id          = ID,
                      owner       = _Owner,
                      name        = Name,
                      ingredients = _Ingre,
                      directions  = _Direc}) ->
    RecipeURL = ["recipe/",ID],
    #panel{'class' = "recipe",
          body = [ 
                  #link{text = Name,
                        url  = RecipeURL
                    }
                 ]}.

-spec(get_recipe_by_id(recipe_id()) ->
             {ok,#recipe{}}| not_found).
get_recipe_by_id(ID) ->
    [Recipe] = mnesia:dirty_match_object(recipe,#recipe{id=ID, _='_'}),
    {ok, Recipe}.

list_recipes_by_user(UserId) ->
    mnesia:dirty_match_object(recipe, #recipe{owner= UserId, _='_'}).

 
