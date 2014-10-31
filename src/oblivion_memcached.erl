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

-define(UNSUPPORTED_OPERATIONS, [?OPCODE_INCREMENT, ?OPCODE_DECREMENT, ?OPCODE_INCREMENT_QUIET, ?OPCODE_DECREMENT_QUIET,
                                 ?OPCODE_APPEND, ?OPCODE_PREPEND, ?OPCODE_APPEND_QUIET, ?OPCODE_PREPEND_QUIET,
                                 ?OPCODE_VERBOSITY, ?OPCODE_SASL_LIST_MECHS, ?OPCODE_SASL_AUTH, ?OPCODE_SASL_STEP,
                                 ?OPCODE_R_GET, ?OPCODE_R_SET, ?OPCODE_R_SET_QUIET, ?OPCODE_R_APPEND, ?OPCODE_R_APPEND_QUIET,
                                 ?OPCODE_R_PREPEND, ?OPCODE_R_PREPEND_QUIET, ?OPCODE_R_DELETE, ?OPCODE_R_DELETE_QUIET,
                                 ?OPCODE_R_INCREMENT, ?OPCODE_R_INCREMENT_QUIET, ?OPCODE_R_DECREMENT, ?OPCODE_R_DECREMENT_QUIET,
                                 ?OPCODE_SET_VBUCKET, ?OPCODE_GET_VBUCKET, ?OPCODE_DELETE_VBUCKET, ?OPCODE_TAP_CONNECT,
                                 ?OPCODE_TAP_MUTATION, ?OPCODE_TAP_DELETE, ?OPCODE_TAP_FLUSH, ?OPCODE_TAP_OPAQUE,
                                 ?OPCODE_TAP_VBUCKET_SET, ?OPCODE_TAP_CHECKPOINT_START, ?OPCODE_TAP_CHECKPOINT_END]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	error_logger:info_msg("Aqui estou eu no init\n", []), %% TODO DEBUG
	ok = ranch:accept_ack(Ref),
	
	%% TODO Inicializacao do Gibreel -> esta aqui temporariamente porque o init e chamado sempre que ha uma nova ligacao ao servidor.
	%% TODO nome da cache e configuravel
	CacheName = aminhacache,
	gibreel:create_cache(CacheName),
	
	loop(Socket, Transport, CacheName).

loop(Socket, Transport, CacheName) ->
	try
		case Transport:recv(Socket, ?HEADER_SIZE_REQUEST, 60000) of %% TODO Configurar o timeout de espera
			{ok, Data} ->
				{ok, Response} = process_request(Data, Transport, Socket, CacheName),
				%% TODO A reposta pode ser 0 ou mais pacotes
				error_logger:info_msg("Response to send ~p \n", [Response]), %% TODO DEBUG
				ok = Transport:send(Socket, Response),
				error_logger:info_msg("Response sent\n", []), %% TODO DEBUG
				loop(Socket, Transport, CacheName);
			_ -> ok = Transport:close(Socket)
		end
	catch
		_:Error ->
			error_logger:info_msg("Unexpected error during request processing [~p]\n", [Error]),
			ok = Transport:close(Socket)
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================
%% This functions exists because recv with 0 length return the all the available bytes.
internal_recv(_Transport, _Socket, 0) -> {ok, <<>>};
internal_recv(Transport, Socket, Length) ->
	case Transport:recv(Socket, Length, 30000) of %% TODO Configurar este timeout
		{ok, Data} -> {ok, Data};
		{error, Error} -> throw({communication_error, Error})
	end.

%%
%% Response building functions
%%
get_error_response(Operation, StatusCode, Message, Opaque) ->
	get_response_packets(Operation, StatusCode, <<>>, Message, <<>>, Opaque, ?DEFAULT_DATA_VERSION_CHECK).

%% TODO guardas nas funcoes, para validar os dados que vao ser retornados
get_success_response(Operation, Content, Opaque) ->
	get_success_response(Operation, <<>>, Content, Opaque).
get_success_response(Operation, Key, {Version, Flag, Value}, Opaque) ->
	get_response_packets(Operation, ?RESPONSE_STATUS_NO_ERROR, Key, Value, Flag, Opaque, Version);
get_success_response(Operation, Key, Version, Opaque) ->
	get_response_packets(Operation, ?RESPONSE_STATUS_NO_ERROR, Key, <<>>, <<>>, Opaque, Version).

get_response_packets(Operation, StatusCode, Key, Value, Extras, Opaque, CAS) ->
	ExtrasLength = byte_size(Extras),
	KeyLength = byte_size(Key),
	ValueLength = byte_size(Value),
	TotalBodyLength = ExtrasLength + KeyLength + ValueLength,
	<<?MAGIC_BYTE_RESPONSE:8, Operation:8, KeyLength:16,
	  ExtrasLength:8, ?DATA_TYPE_RAW_BYTES:8, StatusCode:16,
	  TotalBodyLength:32,
	  Opaque:32,
	  CAS:64,
	  Extras/binary, Key/binary, Value/binary>>.

%%
%% Request processing functions
%%
process_request(<<?MAGIC_BYTE_REQUEST:8, Operation:8, KeyLength:16, ExtrasLength:8, _DataType:8, _VBucketId:16, TotalBodyLength:32, Opaque:32, CAS:64>>, Transport, Socket, CacheName) ->
	%% Read the remaining content
	{ok, Extras} = internal_recv(Transport, Socket, ExtrasLength),
	{ok, Key} = internal_recv(Transport, Socket, KeyLength),
	{ok, Value} = internal_recv(Transport, Socket, (TotalBodyLength - (ExtrasLength + KeyLength))),

	process_operation(Operation, Key, Value, Extras, Opaque, CAS, CacheName);
%% If the packet format is unknown, exit
process_request(_Other, _Transport, _Socket, _CacheName) ->
	{error, unknown_request_header_format}.

%% TODO TERMINAR
 process_operation(Operation=?OPCODE_GET, Key, Value, Extras, Opaque, CAS, CacheName) ->
 	error_logger:info_msg("Operation GET ~p | Key ~p | Value ~p | Extras ~p | Opaque ~p | CAS ~p | CacheName ~p\n", [Operation, Key, Value, Extras, Opaque, CAS, CacheName]), %% TODO DEBUG
 
 	Response = case g_cache:get(CacheName, Key) of
 		{ok, Content} -> get_success_response(Operation, Content, Opaque);
 		not_found -> get_error_response(Operation, ?RESPONSE_STATUS_KEY_NOT_FOUND, <<"Not found">>, Opaque);
 		Error ->
 			ErrorBin = atom_to_binary(Error, utf8),
 			Message = <<"Unexpected error querying cache: ", ErrorBin/binary>>,
 			get_error_response(Operation, ?RESPONSE_STATUS_INTERNAL_ERROR, Message, Opaque)
 	end,
 
 	{ok, Response};
 process_operation(Operation=?OPCODE_SET, Key, Value, Extras = <<Flag:32, Expiration:32>>, Opaque, CAS, CacheName) ->
 	error_logger:info_msg("Operation SET ~p | Key ~p | Value ~p | Extras ~p | Opaque ~p | CAS ~p | CacheName ~p\n", [Operation, Key, Value, Extras, Opaque, CAS, CacheName]), %% TODO DEBUG
 
 	%% TODO Validacoes da versao (CAS)
 	Response = case g_cache:store(CacheName, Key, {CAS, <<Flag:32>>, Value}) of
 		ok -> get_success_response(Operation, CAS, Opaque);
 		no_cache -> get_error_response(Operation, ?RESPONSE_STATUS_INTERNAL_ERROR, <<"Unexpected error storing values: no_cache">>, Opaque)
 	end,
 
 	{ok, Response};


%% Unsupported and unknown operations
process_operation(Operation, _Key, _Value, _Extras, Opaque, _CAS, _CacheName) ->
	{StatusCode, Message} = case lists:member(Operation, ?UNSUPPORTED_OPERATIONS) of
		true -> {?RESPONSE_STATUS_NOT_SUPPORTED, <<"Unsupported operation">>};
		_ -> {?RESPONSE_STATUS_UNKNOWN_COMMAND, <<"Unknown operation">>}
	end,
	error_logger:info_msg("Operation ~p | StatusCode ~p | Message ~p \n", [Operation, StatusCode, Message]), %% TODO DEBUG
	{ok, get_error_response(Operation, StatusCode, Message, Opaque)}.
