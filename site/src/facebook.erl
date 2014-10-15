%% -*- mode: nitrogen -*-
-module (facebook).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

api_event(Name, Tag, [Evt]) ->
    io:format("api_event(~p,~p,~p)~n",[Name,Tag, Evt]),
    Data = jsx:decode(list_to_binary(Evt)),
    io:format("Data~p~n", [Data]),
    ok.
        
