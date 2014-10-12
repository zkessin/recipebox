%% -*- mode: nitrogen -*-
-module (user_recipe_list).
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
    ].
