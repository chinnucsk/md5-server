%% Courtesy of learnyousomeerlang.com
%%% Handles socket connections, and bridges a remote server
%%% With a progressquest game.
-module(sockserv_serv).
-behaviour(gen_server).

-record(state, {name, % player's name
                next, % next step, used when initializing
                socket}). % the current socket

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SOCK(Msg), {tcp, _Port, Msg}).
-define(TIME, 800).
-define(EXP, 50).

%% The socket is passed in from sockserv_sup.
%% It's a listen socket, as started by gen_tcp:listen/2.
%%
%% In Erlang, a TCP socket must be started as a listening socket first.
%% The listening socket can then be used to listen for a connection,
%% meant to be accepted. To do so, use gen_tcp:accept/1-2, as it is done
%% later in this module.
%%
%% A single listen socket can be used by many processes, each accepting
%% a communication. When a communication is accepted with accept/1-2,
%% a new socket, called accept socket, is returned. This accept socket
%% is the one that may be used to communicate with a client.
start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
%% properly seeding the process
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
%% Because accepting a connection is a blocking function call,
%% we can not do it in here. Forward to the server loop!
  gen_server:cast(self(), accept),
  {ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
  {noreply, State}.

%% Accepting a connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
%% this is the socket acceptance mentioned earlier
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
%% Remember that thou art dust, and to dust thou shalt return.
%% We want to always keep a given number of children in this app.
  md5app_srv:start_socket(), % a new acceptor is born, praise the lord
  {noreply, S#state{socket=AcceptSocket, next=name}}.

handle_info(?SOCK(Binary), S = #state{socket=Socket}) ->
  ToHashStr = binary_to_list(Binary),
  {hash,Hash} = md5app:get_hash(ToHashStr),
  ok = send(Socket, Hash, []),
  gen_tcp:close(Socket),
  {noreply, S};
handle_info({tcp_closed, _Socket}, S) ->
  {stop, normal, S};
handle_info({tcp_error, _Socket}, S) ->
  {stop, normal, S};
handle_info(E, S) ->
  io:format("unexpected: ~p~n", [E]),
  {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok;
terminate(_Reason, _State) ->
  io:format("terminate reason: ~p~n", [_Reason]).

send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok.
