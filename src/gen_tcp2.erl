-module(gen_tcp2).

-export([accept/1, accept/2, close/1, connect/3,
	 connect/4, listen/2, recv/1, recv/2, recv/3, send/2]).

listen(Port, Opts) ->
    {ok, LSock} = socket:open(inet, stream, tcp),
    Addr = proplists:get_value(ip, Opts, any),
    {ok, OutPort} = socket:bind(LSock,
				#{family => inet, port => Port, addr => Addr}),
    ok = socket:listen(LSock),
    {ok, LSock, OutPort}.

% connect(SAddr, SPort, Opts) ->
%     connect(SAddr, SPort, Opts, infinity).

% connect(localhost, SPort, Opts, Timeout) ->
%     connect(loopback, SPort, Opts, Timeout);
% connect(SAddr, SPort, Opts, Timeout) ->
%     {ok, Sock} = socket:open(inet, stream, tcp),
%     Addr = proplists:get_value(ip, Opts, any),
%     {ok, _} = socket:bind(Sock,
% 			  #{family => inet, addr => Addr}),
%     ok = socket:connect(Sock,
% 			#{family => inet, addr => SAddr, port => SPort},
% 			Timeout),
%     {ok, Sock}.

send(Sock, Data) -> socket:send(Sock, Data).

recv(Sock) -> socket:recv(Sock).

recv(Sock, Length) -> socket:recv(Sock, Length).

recv(Sock, Length, Timeout) ->
    socket:recv(Sock, Length, Timeout).

close(Sock) -> socket:close(Sock).
