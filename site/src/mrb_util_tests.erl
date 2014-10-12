-module(mrb_util_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(recipe, {
          id          :: binary(), %% GUID
          owner       :: binary(),

          name        :: string(),
          ingredients :: string(),
          directions  :: string()}).

owner() ->
    oneof([<<"aaaaaaaaa">>,
           <<"bbbbbbbbb">>,
           <<"ccccccccc">>]).

generate_recipe() ->
    generate_recipe(owner()).

generate_recipe(Owner) ->
    #recipe{ owner       = Owner,
             id          = vector(16, range(65,96)),
             name        = string(),
             ingredients = string(),
             directions  = string()}.

prop_save_and_retrieve() ->
    ?FORALL(RECIPE = #recipe{ id = ID},
            generate_recipe(),
            begin
                ok = mrb_util:add_recipe(RECIPE),
                {ok, R} = mrb_util:get_recipe_by_id(ID),
                R =:= RECIPE
            end).

prop_get_and_list() ->
    UserId = <<"test owner">>,
    ?FORALL(RECIPES,
            list(generate_recipe(UserId)),
            begin
                {atomic, ok} = mnesia:clear_table(recipe),
                mrb_util:add_recipe(RECIPES),
                Rs = mrb_util:list_recipes_by_user(UserId),
                ?assert(is_list(Rs)),
                ?assertEqual(length(Rs), length(RECIPES)),
                ?assertEqual(lists:sort(Rs),lists:sort( RECIPES)),
                true
            end).
              
run_props_test() ->
    application:ensure_all_started(mnesia),
    mrb_util:setup_mnesia_tables(),
    ?assert(  
       proper:quickcheck(mrb_util_tests:prop_get_and_list(), [{to_file, user}])),

    {atomic, ok} = mnesia:clear_table(recipe),
    ?assert(  
       proper:quickcheck(mrb_util_tests:prop_save_and_retrieve(), [{to_file, user}])).
 
