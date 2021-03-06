%% Copyright 2015, Travelping GmbH <copyright@travelping.com>

-module(click_dp_app_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Groups) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Groups]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Groups]) ->
    ClientSpecs =  [?CHILD(click_dp_mock, worker, []),
		    ?CHILD(click_dp, worker, [Groups])],
    {ok, {{one_for_all, 5, 10}, ClientSpecs}}.
