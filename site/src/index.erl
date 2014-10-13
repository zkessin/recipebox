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
                          #panel{
                             body=recipe_list() },
                          #grid_6 {
                             alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
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


recipe_list() ->
    Recipes = mrb_util:list_recipes_by_user(<<"test">>),
    io:format("Recipes ~p~n", [Recipes]),
    HTML    = [mrb_util:format_recipe(R) || R <- Recipes],
    io:format("HTML ~p~n",[HTML]),
    HTML.

get_recipe() ->
    Owner = "test",
    Name  = wf:q(name), 
    Ingre = wf:q(ingredients),
    Direc = wf:q(directions),
    {ok, Res} = mrb_util:make_recipe(Owner, Name, Ingre, Direc),
    {ok, Res}.

event(submit) ->

    {ok,Recipe} = get_recipe(),
    ok          = mrb_util:add_recipe(Recipe),

    wf:replace(button, #panel { 
                          body=submit, 
                          actions=#effect { effect=highlight }
                         });
event(Evt) ->
    io:format("Evt ~p~n",[Evt]),
    ok.
