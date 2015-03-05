%% ----------------------------------
%% @doc Mock Click DP module.
%%
%% @end
%% ----------------------------------

-module(click_dp_mock).

-behavior(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, 'click').

%%===================================================================
%% API
%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%===================================================================
%% gen_server callbacks
%%===================================================================
%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(Request, From, State) ->
    lager:info("got call (~p, ~p)", [Request, From]),
    {reply, ok, State}.

%% @private
handle_cast(Request, State) ->
    lager:info("got cast ~p", [Request]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    lager:warning("got info ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
