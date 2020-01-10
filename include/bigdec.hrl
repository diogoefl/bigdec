%%%----------------------------------------------------------------------------
%%% @copyright (diogoefl) 2020. All Rights Reserved.
%%% @author diogoefl
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" basis,
%%% without warranties or conditions of any kind, either express or
%%% implied. See the License for the specific language governing permissions
%%% and limitations under the License.
%%%
%%% @doc Arbitrary Precision Decimal library header file
%%% @end
%%%
%%%----------------------------------------------------------------------------

%%=============================================================================
%% API
%%=============================================================================

%%=============================================================================
%% Data Structures
%%=============================================================================

-record(bigdec, {sign  = 0 :: 0 | 1,
                 value = 0 :: non_neg_integer(),
                 exp   = 0 :: non_neg_integer()}).

%%=============================================================================
%% Macros
%%=============================================================================
