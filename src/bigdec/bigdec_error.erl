%%%= LICENSE INFORMATION START ========================================================================================
%%% BigDec Library
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
%%% the License. You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
%%% an "AS IS" basis, without warranties or conditions of any kind, either express or implied. See the License for the
%%% specific language governing permissions and limitations under the License.
%%%= LICENSE INFORMATION END===========================================================================================

%%%--------------------------------------------------------------------------------------------------------------------
%%% @doc Arbitrary Precision Decimal Invalid and Errors.
%%%
%%% Defines different data and errors when invalid numbers are used.
%%% @author diogoefl
%%% @copyright (diogoefl) 2020. All Rights Reserved.
%%% @end
%%%--------------------------------------------------------------------------------------------------------------------
-module(bigdec_error).
%% @headerfile ["bigdec.hrl"]

%%%====================================================================================================================
%%% Data Structures
%%%====================================================================================================================
-include("bigdec.hrl").

%%%====================================================================================================================
%%% Module setup
%%%====================================================================================================================
-export([division_by_zero/0, division_impossible/0, division_undefined/0, invalid_option/0]).

%%%====================================================================================================================
%%% EUnit setup
%%%====================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%====================================================================================================================
%%% Library public functions - Constants
%%%====================================================================================================================

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Division by 0.
%%
%% When this occurs, operation signals an error result, containing infinity as fallback value. Infinity value is a
%% bigdec element with value as atom inf.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
division_by_zero() ->
  {error, division_by_zero, infinity}.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Cannot perform the division adequately.
%%
%% When this occurs, operation signals invalid operation if the integer result of a divide-integer or remainder
%% operation had too many digits that would be longer than precision is capable of calculating.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
division_impossible() ->
  {error, division_impossible, not_a_number}.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Undefined result of division.
%%
%% This occurs and signals invalid operation if division by zero was attempted, and dividend is also zero.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
division_undefined() ->
  {error, division_undefined, not_a_number}.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Invalid options.
%%
%% This occurs and signals invalid operation if an invalid option for context handling was detected during an
%% operation. This might happen for example if unknown rounding is reached.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
invalid_option() ->
  {error, invalid_option, not_a_number}.

%%%====================================================================================================================
%%% EUnit Tests
%%%====================================================================================================================
-ifdef(TEST).

-endif.