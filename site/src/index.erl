%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).

-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "My RecipeBox".



body() ->
    mrb_facebook:facebook_postback(),
    SessionId =  mrb_facebook:get_session_user_id(),
    #panel{ id   = body,
            body = make_body(SessionId)}.

make_body({ok, UserID}) ->
    lager:info("Session ID ~p~n", [UserID]),
    #container_12 { body=[

                          #table{
                             id     = recipe_table,
                             rows   = recipe_list(UserID)
                            },
                          #grid_6 {
                             alpha  = true, 
                             prefix = 2, 
                             suffix = 2, 
                             omega  = true, 
                             body   = inner_body(UserID) }
                         ]};
make_body(not_found) ->
    #container_12{body = [
                    #grid_6{
                             alpha  = true, 
                             prefix = 2, 
                             suffix = 2, 
                             omega  = true, 
                             body   = signon_body() }
                    

                   ]}.

signon_body() ->
    [#h1{ text = "Signin with facebook"}].

inner_body(UserID) -> 
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


recipe_list(Owner) ->
    Header  = mrb_util:recipe_header(),
    Recipes = mrb_util:list_recipes_by_user(Owner),
    HTML    = [mrb_util:format_recipe(R) || R <- Recipes],
    [Header|HTML].


get_recipe_from_post(Owner) ->
    Name       = ?GET(recipe_name), 
    Ingre      = ?GET(ingredients),
    Direc      = ?GET(directions),
    {ok, Res}  = mrb_util:make_recipe(Owner, Name, Ingre, Direc),
    lager:info("Recipe ~p", [Res]),
    {ok, Res}.


event(submit) ->
    {ok,UserId} = mrb_facebook:get_session_user_id(),
    {ok,Recipe} = get_recipe_from_post(UserId),
    ok          = mrb_util:add_recipe(Recipe),
    wf:replace(recipe_table,
               #panel { 
                  body    =  recipe_list(UserId), 
                  actions = #effect { effect=highlight }
                 });
event(Evt) ->
    io:format("Evt ~p~n",[Evt]),
    ok.
