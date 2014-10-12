%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-record(recipe, {name        :: string(),
                 ingredients :: string(),
                 directions  :: string()}).

-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "My RecipeBox".

body() ->
    #container_12 { body=[
        #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
    ]}.

inner_body() -> 
    [
     #h1 { text="My Recipe Box" },
     #p{},
     #restful_form{
        method  = post,
        enctype = "application/json",
        body    = [
                   #textbox { 
                      id       = name,
                      text     = "Recipe Name", 
                      next     = ingredients
                     },
                   #textarea {
                      id       = ingredients, 
                      text     = "Ingredients one per line",
                      columns  = 70,
                      next     = directions
                     },
                   #br{},
                   #textarea { 
                      id       = directions,    
                      text     = "Directions",
                      columns  = 70,
                      next     = submit
                     },
                   #br{},
                   #button{
                      id       = submit, 
                      postback = submit,
                      text     = "Submit"
                     }
                  ]}
    ].

-spec(get_recipe() -> {ok, #recipe{}}).
get_recipe() ->	
    [Name, Ingredients, Directions] = wf:mq([name,ingredients, directions]),
    {ok,#recipe{name        = Name,
                ingredients = Ingredients,
                directions  = Directions}}.

-spec(save_recipe(#recipe{}) -> ok).
save_recipe(Recipe) when is_record(Recipe, recipe)->
    {atomic, _} = mnesia:transaction(fun() ->
                                             mnesia:write(Recipe)
                                     end),
    ok.

event(EVT) ->
    {ok,Recipe} = get_recipe(),
    io:format("Recipe ~p~n",[Recipe]),
    ok          = save_recipe(Recipe),

    wf:replace(button, #panel { 
                          body=EVT, 
                          actions=#effect { effect=highlight }
                         }).
