-module(mrb_util_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").


owner() ->
    {owner_id, oneof([<<"aaaaaaaaa">>,
                     <<"bbbbbbbbb">>,
                     <<"ccccccccc">>])}.

generate_recipe() ->
    generate_recipe(owner()).


generate_recipe(Owner) ->
    #recipe{ owner       = Owner,
             id          = mrb_util:make_uurecipe_id(),
             name        = {recipe_name, binary()},
             ingredients = {ingredients, binary()},
             directions  = {directions,  binary()}}.


prop_save_and_retrieve() ->
    ?FORALL(RECIPE = #recipe{ id = ID},
            generate_recipe(),
            begin
                ok      = mrb_util:delete_recipe(ID),
                ok      = mrb_util:add_recipe(RECIPE),
                {ok, R} = mrb_util:get_recipe_by_id(ID),
                R =:= RECIPE
            end).

prop_get_and_list() ->
    UserId = {owner_id,<<"test owner">>},
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
              
run_props_test_() ->
    {setup,
     fun() ->
             application:ensure_all_started(mnesia),
             mrb_util:setup_mnesia_tables()
     end,
     begin
         Props = [prop_get_and_list(), prop_save_and_retrieve()],
         [begin
              mnesia:clear_table(recipe),
              
              ?_assert(proper:quickcheck(Prop, [{to_file, user}]))
          end|| Prop <-Props
          
         ]
     end}.
 
