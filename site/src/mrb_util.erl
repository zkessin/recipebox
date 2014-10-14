-module(mrb_util).
-include_lib("eunit/include/eunit.hrl").

-include("records.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.
-export([list_recipes_by_user/1, add_recipe/1, get_recipe_by_id/1, format_recipe/1,recipe_header/0 , make_uurecipe_id/0, delete_recipe/1]).

-spec(make_uurecipe_id() ->recipe_id()).
make_uurecipe_id() ->
    V4    = uuid:get_v4_urandom_native(),
    StrV4 = uuid:uuid_to_string(V4),
    {recipe_id, list_to_binary(StrV4)}.

-spec(make_recipe(owner_id(), recipe_name(), ingredients(), directions()) ->
             {ok, #recipe{}}).
make_recipe(Owner, Name, Ingredients, Directions) ->
    {ok, #recipe{ 
            id          = make_uurecipe_id(),
            owner       = Owner,
            name        = Name,
            ingredients = Ingredients,
            directions  = Directions}}.

-spec(setup_mnesia_tables() ->ok).
setup_mnesia_tables() ->
    case mnesia:create_table(recipe, [ {type, bag},
                                       {attributes, record_info(fields,recipe)}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _}} ->
            ok
        
    end.

-spec(add_recipe(maybe_list(#recipe{})) -> ok).
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

-spec(delete_recipe(recipe_id()) -> ok).
delete_recipe({recipe_id, _} = RecipeID) ->
    ok = mnesia:dirty_delete(recipe, RecipeID),
    ok.

-spec(recipe_header() ->nitrogen_element()).
recipe_header() ->
    #tablerow{'class' = "recipe",
          cells = [ 
                    #tableheader{text = "Name"},
                    #tableheader{text = "Ingredients"},
                    #tableheader{text = "Directions"}
                 ]}.

-spec(format_recipe(#recipe{}) -> nitrogen_element()).
format_recipe(#recipe{id          = {recipe_id,   ID},
                      owner       = {owner_id,    _Owner},
                      name        = {recipe_name, Name},
                      ingredients = {ingredients, Ingre},
                      directions  = {directions,  Direc}}) ->
%    RecipeURL = ["recipe/",ID],
    #tablerow{'class' = "recipe",
          cells = [ 
                    #tablecell{text = Name},
                    #tablecell{text = Ingre},
                    #tablecell{text = Direc}
                 ]}.

-spec(get_recipe_by_id(recipe_id()) ->
             maybe(#recipe{})).
get_recipe_by_id(ID) ->
    case mnesia:dirty_match_object(recipe,#recipe{id=ID, _='_'}) of
        [Recipe] ->
            {ok, Recipe};
        [] ->
            not_found
    end.

-spec(list_recipes_by_user(owner_id()) ->[#recipe{}]).
list_recipes_by_user(UserId) ->
    mnesia:dirty_match_object(recipe, #recipe{owner= UserId, _='_'}).

 
