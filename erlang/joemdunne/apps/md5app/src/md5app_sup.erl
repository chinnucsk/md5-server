-module(md5app_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
  VMaster = { md5app_vnode_master,
	      {riak_core_vnode_master, start_link, [md5app_vnode]},
	      permanent, 5000, worker, [riak_core_vnode_master]},
  TcpServer = { md5app_server,
		{md5app_srv, start_link, []},
		permanent, 5000, worker, [md5app_server]},

  { ok,
    { {one_for_one, 5, 10},
      [VMaster,TcpServer]}}.
