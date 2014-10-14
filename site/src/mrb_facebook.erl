-module(mrb_facebook).

-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-spec(setup_mnesia_tables() ->ok).
setup_mnesia_tables() ->
    case mnesia:create_table(user, [ {type, set},
                                     {attributes, record_info(fields,user)}]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, _}} ->
            ok
        
    end.

-spec(save_user(user_rec()) ->ok).
save_user(User) when is_record(User, user)->
    Trans = fun() ->
                    mnesia:write(User)
            end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.
    
-spec(lookup_user_by_id(owner_id()) -> maybe(user_rec())).
lookup_user_by_id({owner_id,_} = UserID) ->
    
    case mnesia:dirty_match_object(user,#user{user_id=UserID, _='_'}) of
        [User] ->
            {ok, User};
        [] ->
            not_found
    end.
 
-spec(get_user_firstname(user_rec()) -> binary()).
get_user_firstname(#user{first_name = Name}) ->
    {first_name, FName} = Name,
    FName.

-spec(format_user(user_rec()) -> nitrogen_element()).
format_user(User) when is_record(User, user) ->
    #panel{}.