-module(mrb_facebook_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").
-compile(export_all).



prop_save_and_retrieve_user() ->
    ?FORALL(User = #user{user_id = ID},
            user_rec(), 
            ?IMPLIES(is_defined(ID),
                     begin
                         mnesia:clear_table(user),
                         mrb_facebook:save_user(User),
                         {ok, User} = mrb_facebook:lookup_user_by_id(ID),
                         true
                     end)).


prop_get_user_first_name() ->
    ?FORALL(User = #user{first_name = FirstName},
            user_rec(), 
            ?IMPLIES(is_defined(FirstName),
                     begin
                         Name = mrb_facebook:get_user_firstname(User),
                         {first_name,Name} =:= FirstName
                     end)).
                         
is_defined('_') ->
    false; 
is_defined(undefined) ->
    false;
is_defined(_) ->
    true.
%--------------------------------------------------------------------------------
run_spec_test_() ->
    Funs = [{ mrb_facebook,format_user,1}],
    [
     ?_assert(proper:check_spec(Fun, []))|| Fun <-Funs
    ]. 


 
run_props_test_() ->
    {setup,
     fun() ->
             application:ensure_all_started(mnesia),
             mrb_facebook:setup_mnesia_tables()
     end,
     begin
         Props = [prop_save_and_retrieve_user(), prop_get_user_first_name()],
         [begin
              ?_assert(proper:quickcheck(Prop, []))
          end|| Prop <-Props
         ]
     end}.

