%% Copyright 2015, Travelping GmbH <info@travelping.com>

-module(scg_click_dp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    scg_click_dp_app_sup:start_link().

stop(_State) ->
    ok.
