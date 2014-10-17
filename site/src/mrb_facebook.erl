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
    lager:info("save_user(~p)~n", [User]),
    {atomic, ok} = mnesia:transaction(Trans),
    ok.
    
-spec(lookup_user_by_id(user_id()) -> maybe(user_rec())).
lookup_user_by_id({user_id,_} = UserID) ->
    case mnesia:dirty_match_object(user,#user{user_id=UserID, _='_'}) of
        [User] ->
            {ok, User};
        [] ->
            not_found
    end.
-spec(save_session_user_id(user_rec()) ->ok).
save_session_user_id(#user{user_id = ID}) ->
    wf:session(facebook_user_id, ID),
    ok.

-spec(get_session_user_id() -> maybe(user_id())).
get_session_user_id() ->
    case wf:session_default(facebook_user_id, not_found) of
        UserId = {user_id, _} ->
            {ok, UserId};
        not_found ->
            not_found
    end.

-spec(get_user_firstname(user_rec()) -> binary()).
get_user_firstname(#user{first_name = Name}) ->
    {first_name, FName} = Name,
    FName.

-spec(format_user(user_rec()) -> nitrogen_element()).
format_user(User) when is_record(User, user) ->
    #panel{}.

facebook_postback() ->
    wf:wire(#api { 
               name     = setup_facebook,
               delegate = facebook
              }). 

-spec(json_to_record(jsx:json_term()) -> error(user_rec())).
json_to_record(Data) ->
    JSON = ?GET_JSON(Data),
    User = #user{
              user_id      = JSON(user_id),
              email        = JSON(email),
              full_name    = JSON(full_name),
              first_name   = JSON(first_name),
              access_token = JSON(access_token)},
    {ok, User}.
