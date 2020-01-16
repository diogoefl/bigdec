%%%--------------------------------------------------------------------------------------------------------------------
%%% BigDec Library Conversions
%%%
%%% @author diogoefl
%%% @copyright (diogoefl) 2020. All Rights Reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
%%% the License. You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
%%% an "AS IS" basis, without warranties or conditions of any kind, either express or implied. See the License for the
%%% specific language governing permissions and limitations under the License.
%%%
%%% @doc Arbitrary Precision Decimal Conversions.
%%%
%%% Defines handlers for converting from and to bigdec.
%%% @end
%%%
%%%--------------------------------------------------------------------------------------------------------------------

-module(bigdec_conv).
%% @headerfile ["bigdec.hrl"]

%%%====================================================================================================================
%%% Data Structures
%%%====================================================================================================================

-include("bigdec.hrl").

%%%====================================================================================================================
%%% Module setup
%%%====================================================================================================================

-export([as_text/1]).

%%%====================================================================================================================
%%% EUnit setup
%%%====================================================================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%====================================================================================================================
%%% Library public functions
%%%====================================================================================================================

%%=============================================================================
%% Library public functions - Conversions to bigdec
%%
%% Planned functions to be implemented
%% => Conversions to bigdec
%% parse(string | bitstring | float | integer)            -> #bigdec{}
%% parse(string | bitstring | float | integer, {options}) -> #bigdec{}
%%=============================================================================


%%=============================================================================
%% Library public functions - Conversions from bigdec
%%
%% Planned functions to be implemented
%% => Conversion from bigdec
%% as_float(#bigdec{})            -> float
%% as_float(#bigdec{}, {options}) -> float
%% as_text(#bigdec{})             -> bitstring           (DONE)
%% as_text(#bigdec{}, {options})  -> bitstring | string
%%
%%=============================================================================

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Convert bigdec representation as text.
%%
%% Using calculation of Value * (Sign * -1) * 10 ^ Exp return the format of a bitstring representing the bigdec data.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec as_text(Number :: bigdec:bigdec()) -> Result :: <<>>.
as_text(Num = #bigdec{}) ->
  #bigdec{sign  = Sign,
          value = Value,
          exp   = Exp   } = bigdec_transform:strip_zeros(Num),
  ValueString             = integer_to_list(Value),
  Length                  = string:length(ValueString),
  LeadingZerosNeeded      = Exp - Length + 1,
  FullValueString         = bigdec_common:hlp_prepend_zeros(ValueString, LeadingZerosNeeded),
  DotLeadingPosition      = string:length(FullValueString) - Exp,
  {IntPart, DecPart}      = lists:split(DotLeadingPosition, FullValueString),
  case Sign of
    0 -> list_to_bitstring(       IntPart ++ "." ++ DecPart);
    1 -> list_to_bitstring("-" ++ IntPart ++ "." ++ DecPart)
  end.

%%%====================================================================================================================
%%% EUnit Tests
%%%====================================================================================================================

-ifdef(TEST).

as_text_test() ->
  ?assertEqual(<<"0.23453">>, as_text(#bigdec{sign = 0, value = 23453, exp = 5})),
  ?assertEqual(<<"-10.54">>,  as_text(#bigdec{sign = 1, value = 10540, exp = 3})),
  ?assertEqual(<<"-0.0001">>, as_text(#bigdec{sign = 1, value =     1, exp = 4})).

-endif.