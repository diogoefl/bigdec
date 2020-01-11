%%%----------------------------------------------------------------------------
%%% BigDec Library
%%%
%%% @author diogoefl
%%% @copyright (diogoefl) 2020. All Rights Reserved.
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
%%%`
%%% @doc Arbitrary Precision Decimal library.
%%%
%%% Defines group of functions for arithmetic, conversion and data structures
%%% when using decimals of arbitrary precision.
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(bigdec).
%% @headerfile ["bigdec.hrl"]

%%=============================================================================
%% Data Structures
%%=============================================================================

-include("bigdec.hrl").
-type     bigdec() :: #bigdec{sign  :: 0 | 1,
                              value :: non_neg_integer(),
                              exp   :: non_neg_integer()}.
%% bigdec() defines tuple object representing a BigDec number. The data
%% structure of bigdec is formed by 3 elements: sign, integer value and
%% exponent. These three elements form the definition of the number based on
%% the following formula: (Sign * -1) * IntValue * (10 ^ Exp).

%%=============================================================================
%% Module setup
%%=============================================================================

%% Library Public API
-export([]).

%% Conversion to bigdec
-export([]).

%% Conversion from bigdec
-export([as_text/1]).

%% Arithmetic
-export([add/2, minus/2]).

%% bigdec_transform module
-export([neg/1, strip_zeros/1]).

%% bigdec_comp module
-export([min/2, max/2, compare/2, is_smaller_or_equal/2, is_greater_or_equal/2,
         is_equal/2, is_smaller/2, is_greater/2, is_zero/1, is_one/1, is_ten/1,
         contains_integer/1, match_exp/2]).

%% bigdec_analysis module
-export([exponent_val/1, unscaled_val/1, sign_val/1, precision_val/1]).

%% bigdec_const module
-export([one/0, zero/0, ten/0]).

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

%%=============================================================================
%% Library public functions - Arithmetic
%%
%% Planned functions to be implemented
%% => Arithmetic
%% add(  #bigdec{}, #bigdec{})  -> #bigdec{} (DONE)
%% minus(#bigdec{}, #bigdec{})  -> #bigdec{} (DONE)
%% mult( #bigdec{}, #bigdec{})  -> #bigdec{}
%% div(  #bigdec{}, #bigdec{})  -> #bigdec{}
%% pow(  #bigdec{}, integer)    -> #bigdec{}
%% rem(  #bigdec{}, #bigdec{})  -> #bigdec{}
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Add two bigdec numbers by matching scale if necessary.
%%
%% If both numbers contain the same exp value, the resulting bigdec is the sum
%% of values adjusted by sign with the same exp. But if exp doesn't match we
%% need to match exp of each number.
%% @end
%%-----------------------------------------------------------------------------
-spec add(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: bigdec().
%% When both numbers are positive and exponent matches
add(#bigdec{sign = 0, value = Value1, exp = Exp},
    #bigdec{sign = 0, value = Value2, exp = Exp}) ->
  strip_zeros(#bigdec{sign = 0, value = Value1 + Value2, exp = Exp});

%% When first number is positive but second is negative, also exponent matches
%% and first number has bigger value than second
add(#bigdec{sign = 0, value = Value1, exp = Exp},
    #bigdec{sign = 1, value = Value2, exp = Exp}) when Value1 >= Value2->
  strip_zeros(#bigdec{sign = 0, value = Value1 - Value2, exp = Exp});

%% When first number is positive but second is negative, also exponent matches
%% and first number has smaller value than second
add(#bigdec{sign = 0, value = Value1, exp = Exp},
    #bigdec{sign = 1, value = Value2, exp = Exp}) when Value1 < Value2 ->
  strip_zeros(#bigdec{sign = 1, value = Value2 - Value1, exp = Exp});

%% If both number are negative and exponent matches
add(#bigdec{sign = 1, value = Value1, exp = Exp},
    #bigdec{sign = 1, value = Value2, exp = Exp}) ->
  strip_zeros(#bigdec{sign = 1, value = Value1 + Value2, exp = Exp});

%% If first number is negative and second number is positive we recursive call
%% to run causes above
add(Num1 = #bigdec{sign = 1}, Num2 = #bigdec{sign = 0}) ->
  add(Num2, Num1);

%% Otherwise we don't have matching exponents and we need to equalize values
%% first and later recur for above cases to make calculation
add(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  {_, MatchedNum1, MatchedNum2} = match_exp(Num1, Num2),
  add(MatchedNum1, MatchedNum2).

%%-----------------------------------------------------------------------------
%% @doc Subtracts two bigdec numbers by matching scale if necessary.
%%
%% The subtraction is a wrapper for inverting sign of Num2 and proceeding with
%% addition. This way we keep logic preserved.
%% @end
%%-----------------------------------------------------------------------------
-spec minus(Number1 :: bigdec(), Number2 :: bigdec()) -> Result :: bigdec().
minus(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  add(Num1, neg(Num2)).

%%=============================================================================
%% bigdec_transform module
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Negate bigdec changing its sign from 0 to 1 or vice-versa.
%%
%% If sign defined in bigdec is invalid (different from 0 or 1), we return a
%% correction bigdec defining sign as 0 (positive of zero).
%% @end
%%-----------------------------------------------------------------------------
-spec neg(Number :: bigdec()) -> Result :: bigdec().
neg(Num = #bigdec{}) -> bigdec_transform:neg(Num).


%%-----------------------------------------------------------------------------
%% @doc Strip possible trailing zeros in bigdec number.
%%
%% If there is any trailing zero, returns a new bigdec with stripped zeros from
%% the right side ov value and adjusted exp value. If no trailing zeros are
%% found in bigdec, the same bigdec is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec strip_zeros(Number :: bigdec()) -> Result :: bigdec().
strip_zeros(Num = #bigdec{}) -> bigdec_transform:strip_zeros(Num).

%%=============================================================================
%% Library public functions - Comparison
%%
%% Planned functions to be implemented
%% => Comparisons
%% max(#bigdec{}, #bigdec{})        -> #bigdec{} (DONE)
%% min(#bigdec{}, #bigdec{})        -> #bigdec{} (DONE)
%% compare(#bigdec{}, #bigdec{})    -> equal | greater | smaller (DONE)
%% is_equal(#bigdec{}, #bigdec{})   -> true | false (DONE)
%% is_smaller(#bigdec{}, #bigdec{}) -> true | false (DONE)
%% is_greater(#bigdec{}, #bigdec{}) -> true | false (DONE)
%% is_smaller_or_equal(#bigdec{}, #bigdec{}) -> true | false (DONE)
%% is_greater_or_equal(#bigdec{}, #bigdec{}) -> true | false (DONE)
%% is_zero(#bigdec{})               -> true | false (DONE)
%% is_one(#bigdec{})                -> true | false (DONE)
%% is_ten(#bigdec{})                -> true | false (DONE)
%% contains_integer(#bigdec{})      -> true | false (DONE)
%% match_exp(#bigdec{}, #bigdec{})  -> integer() (DONE)
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Returns which of the two BigDec Numbers is the greatest in value, and
%% if they are both equals, return first argument.
%% @end
%%-----------------------------------------------------------------------------
-spec max(Number1 :: bigdec(), Number2 :: bigdec()) -> MaxValue :: bigdec().
max(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:max(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Returns which of the two BigDec Numbers is the smallest in value, and
%% if they are both equals, return first argument.
%% @end
%%-----------------------------------------------------------------------------
-spec min(Number1 :: bigdec(), Number2 :: bigdec()) -> MinValue :: bigdec().
min(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:min(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Compares BigDec Number1 with BigDec Number2 and returns atom defining
%% if Number1 is equal, greater or smaller than Number2.
%% @end
%%-----------------------------------------------------------------------------
-spec compare(Number1 :: bigdec(), Number2 :: bigdec()) ->
              Result :: equal | greater | smaller.
compare(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:compare(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Verifies if both bigdec numbers have equal numerical values.
%%
%% Checks if bigdec can be precisely represent the same value as the other, and
%% if they don't match em exponent value, recheck by matching exponents.
%% @end
%%-----------------------------------------------------------------------------
-spec is_equal(Number1 :: bigdec(),
               Number2 :: bigdec()) ->
               Result  :: true | false.
is_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_equal(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Verifies if first argument is smaller than second argument.
%%
%% Checks if bigdec Number1 represents a numeric value smaller than Number2.
%% @end
%%-----------------------------------------------------------------------------
-spec is_smaller(Number1 :: bigdec(),
                 Number2 :: bigdec()) ->
                 Result  :: true | false.
is_smaller(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_smaller(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Verifies if first argument is greater than second argument.
%%
%% Checks if bigdec Number1 represents a numeric value greater than Number2.
%% @end
%%-----------------------------------------------------------------------------
-spec is_greater(Number1 :: bigdec(),
                 Number2 :: bigdec()) ->
                 Result  :: true | false.
is_greater(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:is_greater(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Validates if BigDec Number1 is smaller than or equal to BigDec Number2.
%% @end
%%-----------------------------------------------------------------------------
-spec is_smaller_or_equal(Number1 :: bigdec(), Number2 :: bigdec()) ->
                          Result :: true | false.
is_smaller_or_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  bigdec_comp:is_smaller_or_equal(Num1, Num2).

%%-----------------------------------------------------------------------------
%% @doc Validates if BigDec Number1 is greater than or equal to BigDec Number2.
%% @end
%%-----------------------------------------------------------------------------
-spec is_greater_or_equal(Number1 :: bigdec(), Number2 :: bigdec()) ->
                          Result :: true | false.
is_greater_or_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  bigdec_comp:is_greater_or_equal(Num1, Num2).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Wrapper for bigdec_comp:is_zero/1.
%% @see bigdec_comp:is_zero/1. Implementation at bigdec_comp module.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_zero(Number :: bigdec()) -> Result :: true | false.
is_zero(Num = #bigdec{}) -> bigdec_comp:is_zero(Num).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Wrapper for bigdec_comp:is_one/1.
%% @see bigdec_comp:is_one/1. Implementation at bigdec_comp module.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_one(Number :: bigdec()) -> Result :: true | false.
is_one(Num = #bigdec{}) -> bigdec_comp:is_one(Num).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Wrapper for bigdec_comp:is_ten/1.
%% @see bigdec_comp:is_ten/1. Implementation at bigdec_comp module.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_ten(Number :: bigdec()) -> Result :: true | false.
is_ten(Num = #bigdec{}) -> bigdec_comp:is_ten(Num).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Wrapper for bigdec_comp:contains_integer/1.
%% @see bigdec_comp:contains_integer/1. Implementation at bigdec_comp module.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec contains_integer(Number :: bigdec()) -> Result :: true | false.
contains_integer(Num = #bigdec{}) -> bigdec_comp:contains_integer(Num).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Wrapper for bigdec_comp:match_exp/2.
%% @see bigdec_comp:match_exp/2. Implementation at bigdec_comp module.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec match_exp(Number1 :: bigdec(), Number2 :: bigdec()) -> Result  :: {integer(), bigdec(), bigdec()}.
match_exp(Num1 = #bigdec{}, Num2 = #bigdec{}) -> bigdec_comp:match_exp(Num1, Num2).

%%=============================================================================
%% Library public functions - Analysis
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Value of exponent in bigdec number.
%%
%% The exponent is the component N on the number representation of V * 10 ^ -N.
%% Exponent value in bigdec is neg against the representation, meaning that for
%% example 0.2 is represented as (2 * 10 ^ -1) and the exponent value is +1.
%% @end
%%----------------------------------------------------------------------------
-spec exponent_val(Number :: bigdec()) -> Result :: integer().
exponent_val(Num = #bigdec{}) -> bigdec_analysis:exponent_val(Num).

%%-----------------------------------------------------------------------------
%% @doc Value of unscaled integer representation not applying exponent power.
%%
%% The unscaled value represents the number that multiplied by power of exp by
%% 10 represents the real value of bigdec. The unscaled value is not stripped
%% of trailing zeros.
%% @end
%%----------------------------------------------------------------------------
-spec unscaled_val(Number :: bigdec()) -> Result :: non_neg_integer().
unscaled_val(Num = #bigdec{}) -> bigdec_analysis:unscaled_val(Num).

%%-----------------------------------------------------------------------------
%% @doc Atom defining sign for bigdec.
%%
%% The sign used in bigdec structures use 0 for postive or zero numbers, and 1
%% for negative numbers. This functions returns atom positive if sign is 0 and
%% atom negative if sign is 1. If bigdec gets incorrect data and sign is not 0
%% or 1, than atom invalid is returned.
%% @end
%%----------------------------------------------------------------------------
-spec sign_val(Number :: bigdec()) -> Result :: positive | negative | invalid.
sign_val(Num = #bigdec{}) -> bigdec_analysis:sign_val(Num).

%%-----------------------------------------------------------------------------
%% @doc Amount of digits of the unscaled valued for the bigdec, also known as
%% precision of bigdec.
%%
%% Calculates the amount of digits found in the representation of the unscaled
%% value for the bigdec. This amount is not stripped of trailing zeros.
%% @end
%%----------------------------------------------------------------------------
-spec precision_val(Number :: bigdec()) -> Result :: non_neg_integer().
precision_val(Num = #bigdec{}) -> bigdec_analysis:precision_val(Num).

%%=============================================================================
%% Public API for bigdec_const module
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of one when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec one() -> Result :: bigdec().
one() -> bigdec_const:one().

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of zero when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec zero() -> Result :: bigdec().
zero() -> bigdec_const:zero().

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of ten when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
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

add_test() ->
  %% 0.1 + 0.2 = 0.3
  ?assertEqual(#bigdec{sign = 0, value = 3, exp = 1},
               add(#bigdec{sign = 0, value =  1, exp = 1},
                   #bigdec{sign = 0, value = 20, exp = 2})),
  %% 2.2 + 8.8 = 11
  ?assertEqual(    #bigdec{sign = 0, value =  11, exp = 0},
               add(#bigdec{sign = 0, value =  22, exp = 1},
                   #bigdec{sign = 0, value = 880, exp = 2})),
  %% 10 + 1 = 11
  ?assertEqual(    #bigdec{sign = 0, value = 11, exp = 0},
               add(#bigdec{sign = 0, value = 10, exp = 0},
                   #bigdec{sign = 0, value =  1, exp = 0})),
  %% -5.680 + 3.40 = -2.28
  ?assertEqual(    #bigdec{sign = 1, value =  228, exp = 2},
               add(#bigdec{sign = 1, value = 5680, exp = 3},
                   #bigdec{sign = 0, value =  340, exp = 2})),
  %% -789.457300 - 3.28 = - 792.7373
  ?assertEqual(    #bigdec{sign = 1, value =   7927373, exp = 4},
               add(#bigdec{sign = 1, value = 789457300, exp = 6},
                   #bigdec{sign = 1, value =       328, exp = 2})),
  %% 698 - 437.89 = 260.11
  ?assertEqual(    #bigdec{sign = 0, value = 26011, exp = 2},
               add(#bigdec{sign = 0, value =   698, exp = 0},
                   #bigdec{sign = 1, value = 43789, exp = 2})).

minus_test() ->
  %% 0.3 - 0.205 = 0.095
  ?assertEqual(      #bigdec{sign = 0, value =  95, exp = 3},
               minus(#bigdec{sign = 0, value =   3, exp = 1},
                     #bigdec{sign = 0, value = 205, exp = 3})),
  %% -37.45 - 32 = -69.45
  ?assertEqual(      #bigdec{sign = 1, value = 6945, exp = 2},
               minus(#bigdec{sign = 1, value = 3745, exp = 2},
                     #bigdec{sign = 0, value =   32, exp = 0})),
  %% -37.45 - (-32) = -5.45
  ?assertEqual(      #bigdec{sign = 1, value =  545, exp = 2},
               minus(#bigdec{sign = 1, value = 3745, exp = 2},
                     #bigdec{sign = 1, value =   32, exp = 0})).

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