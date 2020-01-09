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
%%% @doc Arbitrary Precision Decimal library.
%%%
%%% Defines group of functions for arithmetic, conversion and data structures
%%% when using decimals of arbitrary precision.
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(bigdec).

%%=============================================================================
%% Module setup
%%
%% @doc bigdec public API
%% @end
%%=============================================================================

-export([]).

-include("bigdec.hrl").

%%=============================================================================
%% EUnit setup
%%=============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%=============================================================================
%% Library public API
%%=============================================================================


%%=============================================================================
%% Internal Functions - exports for debugging during development
%%=============================================================================

%% Conversions
-export([float_to_bitstring/1, float_to_bitstring/2, bitstring_to_bigdec/1]).

%% Analysis
-export([has_trailing_zeros/1]).

%% Utilities
-export([pow/2]).


%%=============================================================================
%% Internal Functions - Conversions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Converts a float number on a bitstring.
%%
%% If we are on 64 bit system will use 14 decimal places of precision during
%% conversion. Otherwise will use 6 decimal places as precision.
%% @end
%%----------------------------------------------------------------------------
-spec float_to_bitstring(float()) -> bitstring().
float_to_bitstring(FloatNum) ->
  case erlang:system_info(wordsize) of
    8 -> float_to_bitstring(FloatNum, 14);  %% We are on 64bit system
    4 -> float_to_bitstring(FloatNum,  6)
  end.

%%-----------------------------------------------------------------------------
%% @doc Converts a float number on a bitstring based on decimal places.
%%
%% Using amount of decimal places to format a string and later convert this to
%% bitstring. It is advised to use up to 16 decimal cases on 64 bit systems and
%% 7 decimal places for 32 bit systems.
%% @end
%%----------------------------------------------------------------------------
-spec float_to_bitstring(float(), non_neg_integer()) -> bitstring().
float_to_bitstring(FloatNum, DecimalPlaces) ->
  FormatString = "~." ++ integer_to_list(DecimalPlaces) ++ "f",
  list_to_binary(io_lib:format(FormatString, [FloatNum])).

%%-----------------------------------------------------------------------------
%% @doc Converts a bitstring on a bigdecimal record
%%
%% Uses bitstring value containing either float or integer number to convert to
%% bigdec record type.
%% @end
%%-----------------------------------------------------------------------------

bitstring_to_bigdec(Value) ->
  case binary:match(Value, <<".">>) of

    %% We have a float inside the bitstring
    {Index, 1}     -> Length        = erlang:byte_size(Value),
      DecimalPlaces = Length - (Index + 1),
      IntBitString  = binary:replace(Value, <<".">>, <<"">>),
      IntValue      = binary_to_integer(IntBitString),
      case IntValue >= 0 of
        true  -> #bigdec{sign  = 0,
          value = IntValue,
          exp   = DecimalPlaces};
        false -> #bigdec{sign  = 1,
          value = IntValue * -1,
          exp   = DecimalPlaces}
      end;

    %% We have an integer inside the bistring
    nomatch        -> erlang:error(not_implemented)
  end.


%%=============================================================================
%% Internal Functions - Analysis
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Validates if values of bigdec has trailing zeros.
%%
%% Analysis bigdec value to check if it has trailing zeros. If it does return
%% the amount of zeros at the right of the number through a tuple:
%%     -> {true,  AmountOfZeros}
%%      | {false,             0}.
%% @end
%%-----------------------------------------------------------------------------
-spec has_trailing_zeros(#bigdec{}) -> {true, integer()}
                                     | {false,        0}.
has_trailing_zeros(#bigdec{value = Value}) ->
  Amount = has_trailing_zeros(Value, 0),
  case Amount of
    0 -> {false,     0};
    _ -> {true, Amount}
  end.

-spec has_trailing_zeros(integer(), integer()) -> integer().
has_trailing_zeros(Value, Amount) when is_integer(Value),
                                       is_integer(Amount) ->
  case Value rem (pow(10, Amount + 1)) == 0 of
    true  -> has_trailing_zeros(Value, Amount + 1);
    false -> Amount
  end.


%%=============================================================================
%% Internal Functions - Utilities
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Calculates exponentiation by squaring with big integers.
%%
%% Execute exponentiation of numbers, using recursive call of erlang pure ints,
%% but it does not do fractional exponent, or count for negative exponent. At
%% these cases it returns error.
%% @end
%%-----------------------------------------------------------------------------
-spec pow(integer(), non_neg_integer()) -> integer().
pow(Number, Exponent) when is_integer(Number),
                           is_integer(Exponent),
                           Exponent >= 0         ->
  pow(Number, Exponent, 1).

-spec pow(integer(), non_neg_integer(), integer()) -> integer().
pow(  _,   0, Acc)                     -> Acc;
pow(Num,   1, Acc)                     -> Acc*Num;
pow(Num, Exp, Acc) when Exp rem 2 == 0 -> pow(Num*Num, Exp div 2,     Acc);
pow(Num, Exp, Acc)                     -> pow(Num*Num, (Exp-1) div 2, Num*Acc).


%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).

-endif.