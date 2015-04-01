%% Copyright 2015, Travelping GmbH <info@travelping.com>

-module(scg_click_dp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    DEFAULT = 16#55AA,
    Class1 = 1,
    Class2 = 2,
    Class3 = 3,

    Groups = [{{{10,0,1,0},24},Class1},{{{10,0,2,0},24},Class2},{{{10,0,3,0},24},Class3},{{{0,0,0,0},0},DEFAULT}],

    {ok, Pid} = scg_click_dp_app_sup:start_link(Groups),

    Key0 = {inet,<<172,20,48,19>>},
    Key1 = {inet,<<172,20,48,20>>},
    Key2 = {inet,<<172,20,48,21>>},
    Key3 = {inet,<<172,20,48,22>>},
    Client0 = {inet,<<192,168,110,16>>},

    Group = DEFAULT,
    NAT = [
	   {{1,1,1,1}, 'SymetricAddressKeyed', {2,2,2,1}},
	   {{1,1,1,2}, 'AddressKeyed', {2,2,2,2}},
	   {{1,1,1,3}, 'PortKeyed', {{2,2,2,2}, 1000, 2000}},
	   {{1,1,1,4}, 'Random', undefined},
	   {{1,1,1,5}, 'RandomPersistent', undefined},
	   {{1,1,1,6}, 'Masquerade', undefined}
	  ],
    Rules = [
	     {Class1,Class2,accept},
	     {Class2,Class1,drop},
	     {Class1,Class3,deny},
	     {Class1,DEFAULT,accept}
	    ],
    click_dp:insert(Key0, {Group, NAT, Rules}),
    click_dp:insert(Key1, {Group, [],  Rules}),
    click_dp:insert(Key2, {Group, NAT, []}),
    click_dp:insert(Key3, {Group, [],  []}),
    click_dp:insert(Client0, {Class1, [], Rules}),
    {ok, Pid}.


stop(_State) ->
    ok.
