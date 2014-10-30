%%
%% Copyright 2014 Joaquim Rocha <jrocha@gmailbox.org>
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(oblivion_memcached).

-include("oblivion_memcached.hrl").

-behaviour(ranch_protocol).
%% TODO Atencao: se for necessario meter aqui um gen_server, ver as notas em
%% http://ninenines.eu/docs/en/ranch/HEAD/guide/protocols/

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, ?HEADER_SIZE_REQUEST, 60000) of %%TODO 1min de timeout ate receber qq coisa, depois fecha.
		{ok, Data} ->
			case process_request(Data, Transport, Socket) of
				{ok, Response} ->
					%% TODO A reposta pode ser 0 ou mais pacotes
					%% Se status code != nil, o corpo da resposta tem uma mensagem textual
					error_logger:info_msg("Vou enviar uma resposta ~p\n", [Response]), %% TODO DEBUG
					Transport:send(Socket, Response),
					loop(Socket, Transport);
				Error ->
					error_logger:info_msg("Error ~p\n", [Error]),
					ok = Transport:close(Socket)
			end;
		_ ->
			ok = Transport:close(Socket)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% Esta funcao existe porque o recv de 0 retorna tudo o que esta disponivel no Socket
internal_recv(_Transport, _Socket, 0) -> {ok, <<>>};
internal_recv(Transport, Socket, Length) -> Transport:recv(Socket, Length, 1000). %% TODO Configurar este timeout

process_request(<<?MAGIC_BYTE_REQUEST:8, Operation:8, KeyLength:16, ExtrasLength:8, _DataType:8, _VBucketId:16, TotalBodyLength:32, Opaque:32, CAS:64>>, Transport, Socket) ->
	%% Ler o resto do pacote
	{ok, Extras} = internal_recv(Transport, Socket, ExtrasLength),
	{ok, Key} = internal_recv(Transport, Socket, KeyLength),
	{ok, Value} = internal_recv(Transport, Socket, (TotalBodyLength - (ExtrasLength + KeyLength))),
	{ok, Response} = process_operation(Operation, Key, Value, Opaque, CAS),
	{ok, Response};
%% Caso nao seja um pacote conhecido, sair imediatamente
process_request(_Other, _Transport, _Socket) ->
	error.

%% TODO TERMINAR
process_operation(Operation=?OPCODE_GET, Key, Value, Opaque, CAS) ->
	error_logger:info_msg("Operation GET ~p | Key ~p | Value ~p\n", [Operation, Key, Value]), %% TODO DEBUG
	Response = <<?MAGIC_BYTE_RESPONSE:8, Operation:8, 0:16,
	             4:8, 0:8, ?RESPONSE_STATUS_NO_ERROR:16,
	             9:32,
	             Opaque:32,
	             %% CAS:64, %% O CAS aparenta ser a versao dos dados. Confirmar
	             1:64,
	             ?DEFAULT_FLAG_VALUE:32,
	             "World">>,
	{ok, Response};
process_operation(Operation=?OPCODE_SET, Key, Value, Opaque, CAS) ->
	error_logger:info_msg("Operation SET ~p | Key ~p | Value ~p\n", [Operation, Key, Value]), %% TODO DEBUG
	Response = <<?MAGIC_BYTE_RESPONSE:8, Operation:8, 0:16, 0:8, 0:8, 0:16, 0:32, Opaque:32, CAS:64>>,
	{ok, Response};
process_operation(_Operation, _Key, _Value, _Opaque, _CAS) ->
	unknown.
