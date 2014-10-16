%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "My RecipeBox".

body() ->
    mrb_facebook:facebook_postback(),
    #container_12 { body=[

                          #table{
                             id     = recipe_table,
                             rows   = recipe_list()
                            },
                          #grid_6 {
                             alpha  = true, 
                             prefix = 2, 
                             suffix = 2, 
                             omega  = true, 
                             body   = inner_body() }
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
                      id       = recipe_name,
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
    case  mrb_facebook:get_session_user_id() of
        {ok, Owner} ->
            Header  = mrb_util:recipe_header(),
            Recipes = mrb_util:list_recipes_by_user(Owner),
            HTML    = [mrb_util:format_recipe(R) || R <- Recipes],
            [Header|HTML];
        not_found ->
            []
    end.


get_recipe_from_post() ->

   { ok,Owner} = mrb_facebook:get_session_user_id(),
  
    Name       = ?GET(recipe_name), 
    Ingre      = ?GET(ingredients),
    Direc      = ?GET(directions),
    {ok, Res}  = mrb_util:make_recipe(Owner, Name, Ingre, Direc),
    {ok, Res}.


event(submit) ->

    {ok,Recipe} = get_recipe_from_post(),
    ok          = mrb_util:add_recipe(Recipe),

    wf:replace(button,
               #panel { 
%                  body    = submit, 
                  actions = #effect { effect=highlight }
                 });
event(Evt) ->
    io:format("Evt ~p~n",[Evt]),
    ok.
