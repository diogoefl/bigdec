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
%% @doc bigdec library
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

%% Planned API for using bigdec module
%%
%% => Conversions to bigdec
%% parse(string | bitstring | float | integer)            -> #bigdec{}
%% parse(string | bitstring | float | integer, {options}) -> #bigdec{}
%%
%% => Conversion from bigdec
%% as_float(#bigdec{})            -> float
%% as_float(#bigdec{}, {options}) -> float
%% as_text(#bigdec{})             -> bitstring
%% as_text(#bigdec{}, {options})  -> bitstring | string
%%
%% => Arithmetic
%% add(  #bigdec{} | float | integer, #bigdec{} | float | integer)  -> #bigdec{}
%% minus(#bigdec{} | float | integer, #bigdec{} | float | integer)  -> #bigdec{}
%% mult( #bigdec{} | float | integer, #bigdec{} | float | integer)  -> #bigdec{}
%% div(  #bigdec{} | float | integer, #bigdec{} | float | integer)  -> #bigdec{}
%% pow(  #bigdec{}, integer)                                        -> #bigdec{}
%%

%%=============================================================================
%% Internal Functions - exports for debugging during development
%%=============================================================================


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

%% TODO: Segment in parts: processing integer, scientific notation and float
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

-spec bitstring_processing(validate, bitstring()) -> valid | invalid.
bitstring_processing(validate, Value) ->
  Regexp = "[+-]?[0-9]+(\.[0-9]+)?([Ee][+-]?[0-9]+)?",
  Length = string:length(Value),
  case re:run(Value, Regexp) of
    {match, [{0, Length} | _]} -> valid;
    _                          -> invalid
  end.

%%=============================================================================
%% Internal Functions - Analysis
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Validates if values of bigdec has trailing zeros.
%%
%% Analyze bigdec value to check if it has trailing zeros. If it does return
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
  case Value rem (hlp_pow(10, Amount + 1)) == 0 of
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
-spec hlp_pow(integer(), non_neg_integer()) -> integer().
hlp_pow(Number, Exponent) when is_integer(Number),
                            is_integer(Exponent),
                            Exponent >= 0         ->
  hlp_pow(Number, Exponent, 1).

-spec hlp_pow(integer(), non_neg_integer(), integer()) -> integer().
hlp_pow(  _,   0, Acc)                     -> Acc;
hlp_pow(Num,   1, Acc)                     -> Acc*Num;
hlp_pow(Num, Exp, Acc) when Exp rem 2 == 0 -> hlp_pow(Num*Num, Exp div 2,     Acc);
hlp_pow(Num, Exp, Acc)                     -> hlp_pow(Num*Num, (Exp-1) div 2, Num*Acc).


%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).

float_to_bitstring_test() ->
  ?assertEqual(<<"2.20000000000000">>, float_to_bitstring(2.2)),
  ?assertEqual(<<"12232183219874.23400000000000">>, float_to_bitstring(12232183219874.2345672345)),
  ?assertEqual(<<"2.20">>, float_to_bitstring(2.2, 2)),
  ?assertEqual(<<"0.30000000002000">>, float_to_bitstring(0.30000000002)).

bitstring_processing_test() ->
  ?assertEqual(valid,   bitstring_processing(validate, <<"0.1234">>)),
  ?assertEqual(valid,   bitstring_processing(validate, <<"10238573">>)),
  ?assertEqual(valid,   bitstring_processing(validate, <<"1E10">>)),
  ?assertEqual(valid,   bitstring_processing(validate, <<"1223.01239842e-302">>)),
  ?assertEqual(invalid, bitstring_processing(validate, <<"abc">>)),
  ?assertEqual(invalid, bitstring_processing(validate, <<"j12e30">>)),
  ?assertEqual(invalid, bitstring_processing(validate, <<"1234.3e50K">>)),
  ?assertEqual(invalid, bitstring_processing(validate, <<"1234A.934">>)).

has_trailing_zeros_test() ->
  ?assertEqual({true,  3}, has_trailing_zeros(#bigdec{value =       1000})),
  ?assertEqual({true,  3}, has_trailing_zeros(#bigdec{value =      -1000})),
  ?assertEqual({false, 0}, has_trailing_zeros(#bigdec{value = 4392345897})).

pow_test() ->
  ?assertEqual(25,                              hlp_pow(-5,   2)),
  ?assertEqual(4261655511456885005249781170176, hlp_pow(34,  20)),
  ?assertError(function_clause,                 hlp_pow(10, 0.5)),
  ?assertError(function_clause,                 hlp_pow(10,  -2)).

-endif.