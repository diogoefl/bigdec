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
%%% @doc Arbitrary Precision Decimal library API.
%%%
%%% Defines group of functions for arithmetic, conversion and data structures when using decimals of arbitrary
%%% precision. This module wraps functions of other internal modules to provide cleaner API interface for the user.
%%% @author diogoefl
%%% @copyright (diogoefl) 2020. All Rights Reserved.
%%% @end
%%%--------------------------------------------------------------------------------------------------------------------
-module(bigdec).
%% @headerfile ["bigdec.hrl"]

%%%====================================================================================================================
%%% Data Structures
%%%====================================================================================================================
-include("bigdec.hrl").
-export_type([bigdec/0]).
-type     bigdec() :: #bigdec{sign  :: 0 | 1,
                              value :: non_neg_integer(),
                              exp   :: non_neg_integer()}.
%% bigdec() defines tuple object representing a BigDec number. The data structure of bigdec is formed by 3 elements:
%% sign, integer value and exponent. These three elements form the definition of the number based on the following
%% formula: (Sign * -1) * IntValue * (10 ^ Exp).

%%%====================================================================================================================
%%% Module setup
%%%====================================================================================================================
-export([]).

%% Conversion to bigdec
-export([]).

%% Conversion from bigdec
-export([as_text/1]).

%% bigdec_arith module
-export([add/2, minus/2, mult/2]).

%% bigdec_transform module
-export([neg/1, strip_zeros/1]).

%% bigdec_comp module
-export([min/2, max/2, compare/2, is_smaller_or_equal/2, is_greater_or_equal/2, is_equal/2, is_smaller/2, is_greater/2,
         is_zero/1, is_one/1, is_ten/1, contains_integer/1, match_exp/2]).

%% bigdec_analysis module
-export([exponent_val/1, unscaled_val/1, sign_val/1, precision_val/1]).

%% bigdec_const module
-export([one/0, zero/0, ten/0]).

%%%====================================================================================================================
%%% EUnit setup
%%%====================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%====================================================================================================================
%%% Library implementation
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

%%-----------------------------------------------------------------------------
%% @doc Convert bigdec representation as text.
%%
%% Using calculation of Value * (Sign * -1) * 10 ^ Exp return the format of a
%% bitstring representing the bigdec data.
%% @end
%%-----------------------------------------------------------------------------
-spec as_text(Number :: bigdec()) -> Result :: <<>>.
as_text(Num = #bigdec{}) ->
  #bigdec{sign  = Sign,
          value = Value,
          exp   = Exp   } = strip_zeros(Num),
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
%%% Library public API for module bigdec_arith
%%%====================================================================================================================

%% @equiv bigdec_arith:add(Number1, Number2)
-spec add(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: bigdec().
%% When both numbers are positive and exponent matches
add(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_arith:add(Num1, Num2).

%% @equiv bigdec_arith:minus(Number1, Number2)
-spec minus(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: bigdec().
minus(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_arith:minus(Num1, Num2).

%% @equiv bigdec_arith:mult(Number1, Number2)
-spec mult(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: bigdec().
mult(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_arith:mult(Num1, Num2).

%%%====================================================================================================================
%%% Library public API for module bigdec_transform
%%%====================================================================================================================

%% @equiv bigdec_transform:neg(Number)
-spec neg(Number :: bigdec()) -> Result :: bigdec().
neg(Num = #bigdec{}) -> bigdec_transform:neg(Num).

%% @equiv bigdec_transform:strip_zeros(Number)
-spec strip_zeros(Number :: bigdec()) -> Result :: bigdec().
strip_zeros(Num = #bigdec{}) -> bigdec_transform:strip_zeros(Num).

%%%====================================================================================================================
%%% Library public API for module bigdec_comp
%%%====================================================================================================================

%% @equiv bigdec_comp:max(Number1, Number2)
-spec max(Number1 :: bigdec(), Number2 :: bigdec()) -> MaxValue :: bigdec().
max(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:max(Num1, Num2).

%% @equiv bigdec_comp:min(Number1, Number2)
-spec min(Number1 :: bigdec(), Number2 :: bigdec()) -> MinValue :: bigdec().
min(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:min(Num1, Num2).

%% @equiv bigdec_comp:compare(Number1, Number2)
-spec compare(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: equal | greater | smaller.
compare(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:compare(Num1, Num2).

%% @equiv bigdec_comp:is_equal(Number1, Number2)
-spec is_equal(Number1 :: bigdec(), Number2 :: bigdec()) -> Result  :: true | false.
is_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_equal(Num1, Num2).

%% @equiv bigdec_comp:is_smaller(Number1, Number2)
-spec is_smaller(Number1 :: bigdec(), Number2 :: bigdec()) -> Result  :: true | false.
is_smaller(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_smaller(Num1, Num2).

%% @equiv bigdec_comp:is_greater(Number1, Number2)
-spec is_greater(Number1 :: bigdec(), Number2 :: bigdec()) -> Result  :: true | false.
is_greater(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_greater(Num1, Num2).

%% @equiv bigdec_comp:is_smaller_or_equal(Number1, Number2)
-spec is_smaller_or_equal(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: true | false.
is_smaller_or_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_smaller_or_equal(Num1, Num2).

%% @equiv bigdec_comp:is_greater_or_equal(Number1, Number2)
-spec is_greater_or_equal(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: true | false.
is_greater_or_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_greater_or_equal(Num1, Num2).

%% @equiv bigdec_comp:is_zero(Number)
-spec is_zero(Number :: bigdec()) -> Result :: true | false.
is_zero(Num = #bigdec{}) -> bigdec_comp:is_zero(Num).

%% @equiv bigdec_comp:is_one(Number)
-spec is_one(Number :: bigdec()) -> Result :: true | false.
is_one(Num = #bigdec{}) -> bigdec_comp:is_one(Num).

%% @equiv bigdec_comp:is_ten(Number)
-spec is_ten(Number :: bigdec()) -> Result :: true | false.
is_ten(Num = #bigdec{}) -> bigdec_comp:is_ten(Num).

%% @equiv bigdec_comp:contains_integer(Number)
-spec contains_integer(Number :: bigdec()) -> Result :: true | false.
contains_integer(Num = #bigdec{}) -> bigdec_comp:contains_integer(Num).

%% @equiv bigdec_comp:match_exp(Number1, Number2)
-spec match_exp(Number1 :: bigdec(), Number2 :: bigdec()) -> Result  :: {integer(), bigdec(), bigdec()}.
match_exp(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:match_exp(Num1, Num2).

%%%====================================================================================================================
%%% Library public API for module bigdec_analysis
%%%====================================================================================================================

%% @equiv bigdec_analysis:exponent_val(Number)
-spec exponent_val(Number :: bigdec()) -> Result :: integer().
exponent_val(Num = #bigdec{}) -> bigdec_analysis:exponent_val(Num).

%% @equiv bigdec_analysis:unscaled_val(Number)
-spec unscaled_val(Number :: bigdec()) -> Result :: non_neg_integer().
unscaled_val(Num = #bigdec{}) -> bigdec_analysis:unscaled_val(Num).

%% @equiv bigdec_analysis:sign_val(Number)
-spec sign_val(Number :: bigdec()) -> Result :: positive | negative | invalid.
sign_val(Num = #bigdec{}) -> bigdec_analysis:sign_val(Num).

%% @equiv bigdec_analysis:precision_val(Number)
-spec precision_val(Number :: bigdec()) -> Result :: non_neg_integer().
precision_val(Num = #bigdec{}) -> bigdec_analysis:precision_val(Num).

%%%====================================================================================================================
%%% Library public API for module bigdec_const
%%%====================================================================================================================

%% @equiv bigdec_const:one()
-spec one() -> Result :: bigdec().
one() -> bigdec_const:one().

%% @equiv bigdec_const:zero()
-spec zero() -> Result :: bigdec().
zero() -> bigdec_const:zero().

%% @equiv bigdec_const:ten()
-spec ten() -> Result :: bigdec().
ten() -> bigdec_const:ten().

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
-spec float_to_bitstring(Number :: float()) -> Result :: bitstring().
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
-spec float_to_bitstring(Number        :: float(),
                         DecimalPlaces :: non_neg_integer()) ->
                         Result        :: bitstring().
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

-spec bitstring_processing(validate, Text :: bitstring()) -> Result :: valid | invalid.
bitstring_processing(validate, Value) ->
  Regexp = "[+-]?[0-9]+(\.[0-9]+)?([Ee][+-]?[0-9]+)?",
  Length = string:length(Value),
  case re:run(Value, Regexp) of
    {match, [{0, Length} | _]} -> valid;
    _                          -> invalid
  end.

%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).

as_text_test() ->
  ?assertEqual(<<"0.23453">>, as_text(#bigdec{sign = 0, value = 23453, exp = 5})),
  ?assertEqual(<<"-10.54">>,  as_text(#bigdec{sign = 1, value = 10540, exp = 3})),
  ?assertEqual(<<"-0.0001">>, as_text(#bigdec{sign = 1, value =     1, exp = 4})).

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

-endif.