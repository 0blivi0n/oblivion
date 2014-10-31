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

-define(HEADER_SIZE_REQUEST, 24).

-define(MAGIC_BYTE_REQUEST,  16#80).
-define(MAGIC_BYTE_RESPONSE, 16#81).

-define(RESPONSE_STATUS_NO_ERROR,                          16#0000).
-define(RESPONSE_STATUS_KEY_NOT_FOUND,                     16#0001).
-define(RESPONSE_STATUS_KEY_EXISTS,                        16#0002).
-define(RESPONSE_STATUS_VALUE_TO_LARGE,                    16#0003).
-define(RESPONSE_STATUS_INVALID_ARGUMENTS,                 16#0004).
-define(RESPONSE_STATUS_ITEM_NOT_STORED,                   16#0005).
-define(RESPONSE_STATUS_INCR_DECR_ON_NON_NUMERIC,          16#0006).
-define(RESPONSE_STATUS_VBUCKET_BELONGS_TO_ANOTHER_SERVER, 16#0007).
-define(RESPONSE_STATUS_AUTHENTICATION_ERROR,              16#0008).
-define(RESPONSE_STATUS_AUTHENTICATION_CONTINUE,           16#0009).
-define(RESPONSE_STATUS_UNKNOWN_COMMAND,                   16#0081).
-define(RESPONSE_STATUS_OUT_OF_MEMORY,                     16#0082).
-define(RESPONSE_STATUS_NOT_SUPPORTED,                     16#0082).
-define(RESPONSE_STATUS_INTERNAL_ERROR,                    16#0082).
-define(RESPONSE_STATUS_BUSY,                              16#0082).
-define(RESPONSE_STATUS_TEMPORARY_FAILURE,                 16#0082).

-define(OPCODE_GET,                  16#00).
-define(OPCODE_SET,                  16#01).
-define(OPCODE_ADD,                  16#02).
-define(OPCODE_REPLACE,              16#03).
-define(OPCODE_DELETE,               16#04).
-define(OPCODE_INCREMENT,            16#05).
-define(OPCODE_DECREMENT,            16#06).
-define(OPCODE_QUIT,                 16#07).
-define(OPCODE_FLUSH,                16#08).
-define(OPCODE_GET_QUIET,            16#09).
-define(OPCODE_NO_OP,                16#0A).
-define(OPCODE_VERSION,              16#0B).
-define(OPCODE_GET_KEY,              16#0C).
-define(OPCODE_GET_KEY_QUIET,        16#0D).
-define(OPCODE_APPEND,               16#0E).
-define(OPCODE_PREPEND,              16#0F).
-define(OPCODE_STAT,                 16#10).
-define(OPCODE_SET_QUIET,            16#11).
-define(OPCODE_ADD_QUIET,            16#12).
-define(OPCODE_REPLACE_QUIET,        16#13).
-define(OPCODE_DELETE_QUIET,         16#14).
-define(OPCODE_INCREMENT_QUIET,      16#15).
-define(OPCODE_DECREMENT_QUIET,      16#16).
-define(OPCODE_QUIT_QUIET,           16#17).
-define(OPCODE_FLUSH_QUIET,          16#18).
-define(OPCODE_APPEND_QUIET,         16#19).
-define(OPCODE_PREPEND_QUIET,        16#1A).
-define(OPCODE_VERBOSITY,            16#1b).
-define(OPCODE_TOUCH,                16#1c).
-define(OPCODE_GET_AND_TOUCH,        16#1d).
-define(OPCODE_GET_AND_TOUCH_QUIET,  16#1e).
-define(OPCODE_SASL_LIST_MECHS,      16#20).
-define(OPCODE_SASL_AUTH,            16#21).
-define(OPCODE_SASL_STEP,            16#22).
-define(OPCODE_R_GET,                16#30).
-define(OPCODE_R_SET,                16#31).
-define(OPCODE_R_SET_QUIET,          16#32).
-define(OPCODE_R_APPEND,             16#33).
-define(OPCODE_R_APPEND_QUIET,       16#34).
-define(OPCODE_R_PREPEND,            16#35).
-define(OPCODE_R_PREPEND_QUIET,      16#36).
-define(OPCODE_R_DELETE,             16#37).
-define(OPCODE_R_DELETE_QUIET,       16#38).
-define(OPCODE_R_INCREMENT,          16#39).
-define(OPCODE_R_INCREMENT_QUIET,    16#3a).
-define(OPCODE_R_DECREMENT,          16#3b).
-define(OPCODE_R_DECREMENT_QUIET,    16#3c).
-define(OPCODE_SET_VBUCKET,          16#3d).
-define(OPCODE_GET_VBUCKET,          16#3e).
-define(OPCODE_DELETE_VBUCKET,       16#3f).
-define(OPCODE_TAP_CONNECT,          16#40).
-define(OPCODE_TAP_MUTATION,         16#41).
-define(OPCODE_TAP_DELETE,           16#42).
-define(OPCODE_TAP_FLUSH,            16#43).
-define(OPCODE_TAP_OPAQUE,           16#44).
-define(OPCODE_TAP_VBUCKET_SET,      16#45).
-define(OPCODE_TAP_CHECKPOINT_START, 16#46).
-define(OPCODE_TAP_CHECKPOINT_END,   16#47).

-define(DATA_TYPE_RAW_BYTES, 16#00).

-define(DEFAULT_DATA_VERSION_CHECK, 16#00).
