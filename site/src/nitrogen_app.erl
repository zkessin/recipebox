%% -*- mode: nitrogen -*-
-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1, init/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    nitrogen_sup:start_link().

stop(_State) ->
    ok.


init() ->
    sync:go(),
    application:ensure_all_started(mnesia),
    mrb_facebook:setup_mnesia_tables(),
    mrb_util:setup_mnesia_tables().
