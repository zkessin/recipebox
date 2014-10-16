%% -*- mode: nitrogen -*-
-module (facebook).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

api_event(_Name, _Tag, [Evt]) ->
    Data         = jsx:decode(list_to_binary(Evt)),
    {ok,UserRec} = mrb_facebook:json_to_record(Data),
    ok           = mrb_facebook:save_user(UserRec),
    ok           = mrb_facebook:save_session_user_id(UserRec),
    ok.
        
