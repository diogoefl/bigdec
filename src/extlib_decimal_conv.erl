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
%%% @doc Conversion library from and to bigdecimal data record
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(extlib_decimal_conv).

%%=============================================================================
%% Module setup
%%
%% @doc extlib_decimal_conv public API
%% @end
%%=============================================================================

-export([float_to_bitstring/1, float_to_bitstring/2, bitstring_to_bigdec/1]).

-include("extlib_decimal.hrl").

%%=============================================================================
%% Library public API
%%=============================================================================


%%=============================================================================
%% Internal Functions
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Converts a float number on a bitstring.
%%
%% If we are on 64 bit system will use 16 decimal places of precision during
%% conversion. Otherwise will use 7 decimal places as precision.
%% @end
%%----------------------------------------------------------------------------
-spec float_to_bitstring(float()) -> bitstring().
float_to_bitstring(FloatNum) ->
  case erlang:system_info(wordsize) of
    8 -> float_to_bitstring(FloatNum, 16);  %% We are on 64bit system
    4 -> float_to_bitstring(FloatNum,  7)
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