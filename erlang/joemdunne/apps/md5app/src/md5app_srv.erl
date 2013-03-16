%% Courtesy of learnyousomeerlang.com
-module(md5app_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
	 start_socket/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_socket() ->
  supervisor:start_child(?MODULE, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, Port} = application:get_env(md5app,port),
%% Set the socket into {active_once} mode.
%% See sockserv_serv comments for more details
  {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, binary]),
  spawn_link(fun empty_listeners/0),
  {ok, {{simple_one_for_one, 60, 3600},
	[{socket,
	  {sockserv_serv, start_link, [ListenSocket]}, % pass the socket!
	  temporary, 1000, worker, [sockserv_serv]}
	]}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.
