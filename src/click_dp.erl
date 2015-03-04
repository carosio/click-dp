%% ----------------------------------
%% @doc This modules is the Erlang API to the low level data path module.
%%
%% It monitors the DP node, (re)connects to it, restores the working state
%% and wraps the low level DP API's with enhanced Erlang(ish) API's.
%%
%% It also provides API to call the low level DP API directly for development
%% and debug purposes.
%%
%% @end
%% ----------------------------------

-module(click_dp).

-behavior(gen_server).

%% API
-export([start_link/0, call/1, call/2]).

%% generic C-Node wrapper
-export([bind/1, insert/2, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% dev API
-export([run/2]).

-record(state, {state, tref, timeout}).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-define(CNODE_CONF_KEY, 'click-dp').
-define(CNODE_NAME,     "click-dp").
-define(CNODE_SERVER,   'click').

-type int8()  :: 0..255.
%-type int16() :: 0..16#ffff.
%-type int32() :: 0..16#ffffffff.

-type mac() :: <<_:48>>  | {int8(), int8(), int8(), int8(), int8(), int8()}.
-type ip4() :: <<_:32>>  | inet:ip4_address().
-type ip6() :: <<_:128>> | inet:ip6_address().

-type verdict()  :: 'accept' | 'deny' | 'drop'.
-type group()    :: string() | binary().
-type nat_spec() :: term().       %% TODO
-type rule()     :: {group(), group(), verdict()}.

-type key()   :: {'mac', mac()} | {'inet', ip4()} | {'inet6', ip6()}.
-type value() :: {group(), nat_spec(), [rule()]}.

-type error_info() :: {'error', term()} | {'error', term(), term()}.

%%===================================================================
%% API
%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%===================================================================
%% C-Node API Wrapper
%%===================================================================

%% ----------------------------------
%% @doc Bind Owner to the data path, all info message will from now on be delivered to Owner
%% ----------------------------------
bind(Owner) ->
    call({bind, Owner}).

%% ----------------------------------
%% @doc Insert the Values associated with Key into the DP information base.
%%
%% When Key already exists, the associated old values are replaced.
%%
%% If the Key or the Value can not be validated, the function throws badarg.
%% @end
%% ----------------------------------
-spec insert(key(), value()) -> 'ok' | error_info().
insert(Key, Value) ->
    call({insert, key2dp(Key), value2dp(Value)}, 1000).

%% ----------------------------------
%% @doc Delete all values associated with Key from the DP information base.
%%
%% If the Key not be validated, the function throws badarg.
%% ----------------------------------
-spec delete(key()) -> 'ok' | error_info().
delete(Key) ->
    call({delete, key2dp(Key)}, 1000).

call(Args) ->
    call(Args, infinity).

call(Args, Timeout) ->
    gen_server:call(?SERVER, {call, Args, Timeout}, infinity).

%%===================================================================
%% gen_server callbacks
%%===================================================================
%% @private
init([]) ->
    ets:new(?TABLE, [named_table, protected, set, {keypos, 1}]),
    State = connect(#state{state = disconnected, tref = undefined, timeout = 10}),
    {ok, State}.

%% @private
handle_call({call, Request, _Timeout}, _From, State = #state{state = disconnected}) ->
    lager:warning("got call ~p without active data path", [Request]),
    {reply, {error, not_connected}, State};

handle_call({call, Request = {insert, Key, Value}, Timeout}, From, State) ->
    ets:insert(?TABLE, {Key, Value}),
    async_node_call(Request, Timeout, From),
    {noreply, State};

handle_call({call, Request = {delete, Key}, Timeout}, From, State) ->
    ets:delete(?TABLE, Key),
    async_node_call(Request, Timeout, From),
    {noreply, State};

handle_call({call, Request, Timeout}, From, State) ->
    async_node_call(Request, Timeout, From),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast(Request, State = #state{state = disconnected}) ->
    lager:warning("got cast ~p without active data path", [Request]),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({nodedown, Node}, State0) ->
    lager:warning("node down: ~p", [Node]),

    State1 = handle_nodedown(State0),
    State2 = start_nodedown_timeout(State1),
    {noreply, State2};

handle_info(reconnect, State0) ->
    lager:warning("trying to reconnect"),
    State1 = connect(State0#state{tref = undefined}),
    {noreply, State1};

handle_info(Info, State = #state{state = disconnected}) ->
    lager:warning("got info ~p without active data path", [Info]),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("Unhandled info message: ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_node() ->
    DP = application:get_env(scg, ?CNODE_CONF_KEY, ?CNODE_NAME),
    list_to_atom(DP ++ "@" ++ net_adm:localhost()).

start_nodedown_timeout(State = #state{tref = undefined, timeout = Timeout}) ->
    NewTimeout = if Timeout < 3000 -> Timeout * 2;
		    true           -> Timeout
		 end,
    TRef = erlang:send_after(Timeout, self(), reconnect),
    State#state{tref = TRef, timeout = NewTimeout};

start_nodedown_timeout(State) ->
    State.

connect(State) ->
    Node = get_node(),
    case net_adm:ping(Node) of
	pong ->
	    lager:warning("Node ~p is up", [Node]),
	    erlang:monitor_node(Node, true),
	    init_node(State),
	    bind(self()),
	    State#state{state = connected, timeout = 10};
	pang ->
	    lager:warning("Node ~p is down", [Node]),
	    start_nodedown_timeout(State)
    end.

handle_nodedown(State) ->
    State#state{state = disconnected}.

init_node(_State) ->
    Node = get_node(),
    Data = ets:tab2list(?TABLE),
    catch(gen_server:call({?CNODE_SERVER, Node}, {init, Data}, infinity)).

async_node_call(Request, Timeout, From) ->
    proc_lib:spawn_link(fun() -> node_call(Request, Timeout, From) end).

node_call(Request, Timeout, From) ->
    Reply =
	try
	    Node = get_node(),
	    gen_server:call({?CNODE_SERVER, Node}, Request, Timeout)
	catch
	    Class:Error ->
		{error, Class, Error}
	end,
    gen_server:reply(From, Reply).

%%%===================================================================
%%% DP <-> Erlang data mapper
%%%===================================================================

key2dp(Key = {mac, MAC}) when is_binary(MAC) andalso size(MAC) == 6 ->
    Key;
key2dp({mac, {A,B,C,D,E,F}}) ->
    {mac, <<A,B,C,D,E,F>>};
key2dp(Key = {inet, IP}) when is_binary(IP) andalso size(IP) == 4 ->
    Key;
key2dp({inet, {A,B,C,D}}) ->
    {inet, <<A,B,C,D>>};
key2dp(Key = {inet6, IP}) when is_binary(IP) andalso size(IP) == 16 ->
    Key;
key2dp({inet6, {A,B,C,D,E,F,G,H}}) ->
    {inet6, <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>};
key2dp(Key) ->
    erlang:error(badarg, [Key]).

value2dp({Group, NAT, Rules}) ->
    {group2dp(Group), nat2dp(NAT), rules2dp(Rules)};
value2dp(Value) ->
    erlang:error(badarg, [Value]).

group2dp(Group) when is_list(Group) ->
    list_to_binary(Group);
group2dp(Group) when is_binary(Group) ->
    Group;
group2dp(Group) ->
    erlang:error(badarg, [Group]).

nat2dp(NAT) ->
    NAT.

rules2dp(Rules) ->
    lists:foreach(fun rule2dp/1, Rules).

rule2dp({Src, Dst, Verdict}) ->
    {group2dp(Src), group2dp(Dst), verdict2dp(Verdict)};
rule2dp(Rule) ->
    erlang:error(badarg, [Rule]).

verdict2dp(Verdict) when is_atom(Verdict) ->
    Verdict;
verdict2dp(Verdict) ->
    erlang:error(badarg, [Verdict]).

%%%===================================================================
%%% Development helper
%%%===================================================================

run(Key, Value) ->
    bind(self()),
    insert(Key, Value),
    run_loop(Key, Value).

run_loop(Key, Value) ->
    receive
	Other ->
	    io:format("Other: ~p~n", [Other]),
	    ok
    end,
    run_loop(Key, Value).
