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
%% Records and Macros
%%=============================================================================

-record(bigdec, {sign  = 0 :: 0 | 1,
                 value = 0 :: non_neg_integer(),
                 exp   = 0 :: non_neg_integer()}).

-type   bigdec() :: #bigdec{}.
-export_type([bigdec/0]).

%%=============================================================================
%% Module setup
%%=============================================================================

%% Library Public API
-export([]).

%% Conversion to bigdec
-export([]).

%% Conversion from bigdec
-export([]).

%% Arithmetic
-export([add/2, minus/2]).

%% Transform
-export([neg/1, strip_zeros/1]).

%% Comparison
-export([is_zero/1, is_one/1, is_ten/1, contains_integer/1, match_exp/2]).

%% Analysis
-export([exponent_val/1, unscaled_val/1, sign_val/1, precision_val/1]).

%% Constants
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
%% as_text(#bigdec{})             -> bitstring
%% as_text(#bigdec{}, {options})  -> bitstring | string
%%
%%=============================================================================


%%=============================================================================
%% Library public functions - Arithmetic
%%
%% Planned functions to be implemented
%% => Arithmetic
%% add(  #bigdec{}, #bigdec{})  -> #bigdec{} (LACK eunit testing)
%% minus(#bigdec{}, #bigdec{})  -> #bigdec{}
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
%% Library public functions - Transform
%%
%% Planned functions to be implemented
%% => Transform
%% neg(#bigdec{])                                     -> #bigdec{} (DONE)
%% round(#bigdec{})                                   -> #bigdec{}
%% round(#bigdec{}, rounding_pattern)                 -> #bigdec{}
%% change_exp(#bigdec{}, integer())                   -> #bigdec{}
%% change_exp(#bigdec{}, integer(), rounding_pattern) -> #bigdec{}
%% strip_zeros(#bigdec{})                             -> #bigdec{}
%% rescale_by(#bigdec{}, integer())                   -> #bigdec{}
%% incr_exp(#bigdec{})                                -> #bigdec{}
%% decr_exp(#bigdec{})                                -> #bigdec{}
%%
%% => Rounding Patterns
%% round_up        => Increments the digit prior to a nonzero discarded fraction
%% round_down      => Doesn't increment the digit prior to a discarded fraction (trunc)
%% round_ceiling   => Round towards positive infinity - if sign is positive act as
%%                    round_up, if is negative act as round_down
%% round_floor     => Round towards negative infinity - if sign is positive act as
%%                    round_down, it is negative act as round_up
%% round_half_up   => If the discarded fraction is >= 0.5, use round_up
%% round_half_down => If the discarded fraction is >  0.5, use round_up
%% round_half_even => If remainder digit from discard (left digit to the discarded fraction)
%%                    is even, act as round_half_up, otherwise use round_half_down
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Negate bigdec changing its sign from 0 to 1 or vice-versa.
%%
%% If sign defined in bigdec is invalid (different from 0 or 1), we return a
%% correction bigdec defining sign as 0 (positive of zero).
%% @end
%%-----------------------------------------------------------------------------
-spec neg(Number :: bigdec()) -> Result :: bigdec().
neg(Num = #bigdec{value = 0}) -> Num;
neg(Num = #bigdec{sign = 0})  -> Num#bigdec{sign = 1};
neg(Num = #bigdec{sign = 1})  -> Num#bigdec{sign = 0};
neg(Num = #bigdec{sign = _})  -> Num#bigdec{sign = 0}.


%%-----------------------------------------------------------------------------
%% @doc Strip possible trailing zeros in bigdec number.
%%
%% If there is any trailing zero, returns a new bigdec with stripped zeros from
%% the right side ov value and adjusted exp value. If no trailing zeros are
%% found in bigdec, the same bigdec is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec strip_zeros(Number :: bigdec()) -> Result :: bigdec().
strip_zeros(Num = #bigdec{value = Value, exp = Exp}) ->
  case has_trailing_zeros(Num) of
    {false,     0} -> Num;
    {true, Amount} -> Num#bigdec{value = Value div (hlp_pow(10, Amount)),
                                 exp   = Exp - Amount}
  end.

%%=============================================================================
%% Library public functions - Comparison
%%
%% Planned functions to be implemented
%% => Comparisons
%% max(#bigdec{}, #bigdec{})        -> #bigdec{}
%% min(#bigdec{}, #bigdec{})        -> #bigdec{}
%% compare(#bigdec{}, #bigdec{})    -> equal | greater | smaller
%% is_greater(#bigdec{}, #bigdec{}) -> true | false
%% is_equal(#bigdec{}, #bigdec{})   -> true | false
%% is_smaller(#bigdec{}, #bigdec{}) -> true | false
%% is_zero(#bigdec{})               -> true | false (DONE)
%% is_one(#bigdec{})                -> true | false (DONE)
%% is_ten(#bigdec{})                -> true | false (DONE)
%% contains_integer(#bigdec{})      -> true | false (DONE)
%% match_exp(#bigdec{}, #bigdec{})  -> integer() (DONE)
%%
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Validates if bigdec has equivalent value of zero.
%%
%% Checks if bigdec can be precisely represented as zero number, this is
%% validated by first stripping number, and than checking if value and exp are
%% equivalent to zero.
%% @end
%%-----------------------------------------------------------------------------
-spec is_zero(Number :: bigdec()) -> Result :: true | false.
is_zero(Num = #bigdec{}) ->
  %% First we strip the number for later evaluation
  #bigdec{value = Value, exp = Exp} = strip_zeros(Num),
  Value == 0 andalso Exp == 0.

%%-----------------------------------------------------------------------------
%% @doc Validates if bigdec has equivalent value of one.
%%
%% Checks if bigdec can be precisely represented as number one, this is
%% validated by first stripping number, and than checking if value is equal to
%% one, sign equal to zero and exp equal to zero.
%% @end
%%-----------------------------------------------------------------------------
-spec is_one(Number :: bigdec()) -> Result :: true | false.
is_one(Num = #bigdec{}) ->
  %% First we strip the number for later evaluation
  #bigdec{sign = Sign, value = Value, exp = Exp} = strip_zeros(Num),
  Sign == 0 andalso Value == 1 andalso Exp == 0.

%%-----------------------------------------------------------------------------
%% @doc Validates if bigdec has equivalent value of ten.
%%
%% Checks if bigdec can be precisely represented as number ten, this is
%% validated by first stripping number, and than checking if value is equal to
%% ten, sign equal to zero and exp equal to zero.
%% @end
%%-----------------------------------------------------------------------------
-spec is_ten(Number :: bigdec()) -> Result :: true | false.
is_ten(Num = #bigdec{}) ->
  %% First we strip the number for later evaluation
  #bigdec{sign = Sign, value = Value, exp = Exp} = strip_zeros(Num),
  Sign == 0 andalso Value == 10 andalso Exp == 0.

%%-----------------------------------------------------------------------------
%% @doc Validates if the value contained in bigdec represents an integer.
%%
%% Checks if bigdec can be precisely represented as a simple integer, this is
%% validated by check if exp is zero, and also if the amount of trailing zeros
%% is equivalent to the exp value.
%% Function does not use pattern is_* to avoid conflict with BIF is_integer.
%% @end
%%-----------------------------------------------------------------------------
-spec contains_integer(Number :: bigdec()) -> Result :: true | false.
contains_integer(Num = #bigdec{exp = Exp}) ->
  case Exp of
    %% We have no decimal places in bigdec
    0 -> true;
    %% We have decimal places but we need to check trailing zeros
    _ -> case has_trailing_zeros(Num) of
           %% The amount of trailing zeros is equivalent to decimal places
           {true, Exp} -> true;
           %% Any other condition
           _           -> false
         end
  end.

%%-----------------------------------------------------------------------------
%% @doc Compare two bigdec values and define what is the matching exponent.
%%
%% Calculates the common exponent among both bigdec data, but considers first
%% stripping zeros if they exist. Returns the biggest exponent to be used by
%% other functions for calculation, because we can increase exponent without
%% losing precision of data, but not the other way around.
%% @end
%%-----------------------------------------------------------------------------
-spec match_exp(Number1 :: bigdec(), Number2 :: bigdec()) ->
                Result  :: {integer(), bigdec(), bigdec()}.
match_exp(Num1 = #bigdec{},
          Num2 = #bigdec{}) ->
  %% Retrieve the stripped numbers for calculation
  StrippedNum1 = #bigdec{value = Value1, exp = Exp1} = strip_zeros(Num1),
  StrippedNum2 = #bigdec{value = Value2, exp = Exp2} = strip_zeros(Num2),
  %% Returns the equalized numbers matching exp value
  case Exp1 > Exp2 of
    true  -> {Exp1, StrippedNum1,
                    StrippedNum2#bigdec{value = Value2 * hlp_pow(10,Exp1-Exp2),
                                        exp   = Exp1}};
    false -> {Exp2, StrippedNum1#bigdec{value = Value1 * hlp_pow(10,Exp2-Exp1),
                                        exp   = Exp2},
                    StrippedNum2}
  end.

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
exponent_val(#bigdec{exp = Value}) -> Value.

%%-----------------------------------------------------------------------------
%% @doc Value of unscaled integer representation not applying exponent power.
%%
%% The unscaled value represents the number that multiplied by power of exp by
%% 10 represents the real value of bigdec. The unscaled value is not stripped
%% of trailing zeros.
%% @end
%%----------------------------------------------------------------------------
-spec unscaled_val(Number :: bigdec()) -> Result :: non_neg_integer().
unscaled_val(#bigdec{value = Value}) -> Value.

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
sign_val(#bigdec{sign = 0}) -> positive;
sign_val(#bigdec{sign = 1}) -> negative;
sign_val(#bigdec{sign = _}) -> invalid.

%%-----------------------------------------------------------------------------
%% @doc Amount of digits of the unscaled valued for the bigdec, also known as
%% precision of bigdec.
%%
%% Calculates the amount of digits found in the representation of the unscaled
%% value for the bigdec. This amount is not stripped of trailing zeros.
%% @end
%%----------------------------------------------------------------------------
-spec precision_val(Number :: bigdec()) -> Result :: non_neg_integer().
precision_val(#bigdec{value = Value}) ->
  StringVal = integer_to_list(Value),
  string:length(StringVal).

%%=============================================================================
%% Library public functions - Constants
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of one when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec one() -> Result :: bigdec().
one() -> #bigdec{sign = 0, value = 1, exp = 0}.

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of zero when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec zero() -> Result :: bigdec().
zero() -> #bigdec{sign = 0, value = 0, exp = 0}.

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of ten when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec ten() -> Result :: bigdec().
ten() -> #bigdec{sign = 0, value = 10, exp = 0}.

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
%% Internal Functions - Analysis
%%
%% This section provides utility internal functions to validate scenarios.
%% Most of the functions starts with a corresponding questioning has_ is_ .
%% These functions allows modular segmentation from more complex functions and
%% reuse of common patterns.
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Validates if values of bigdec has trailing zeros.
%%
%% Analyze bigdec value to check if it has trailing zeros. Since exp in our
%% data structure is not supposed to be negative, we need to check also if
%% the amount found of trailing zeros is bigger than our exp. It it is the
%% amount to be return needs to be equal to exp. Also if the value of bigdec is
%% equivalent to zero, we can't calculate the amount of trailing zeros so we
%% need to return the exp as result for stripping.
%% @end
%%-----------------------------------------------------------------------------
-spec has_trailing_zeros(Number :: bigdec()) -> Result :: {true, Amount :: integer()}
                                                        | {false,                  0}.
has_trailing_zeros(#bigdec{value = Value, exp = Exp}) ->
  Amount = howmany_trailing_zeros(Value, 0),
  case Amount of
    %% No trailing zeros were found
    0        -> {false,     0};
    %% BigDec value is equivalent to 0
    infinity -> {true,    Exp};
    %% We have trailing zeros, but need to compare with Exp to return smallest
    _        -> case Amount > Exp of
                  true  -> {true,    Exp};
                  false -> {true, Amount}
                end
  end.

%%-----------------------------------------------------------------------------
%% @doc Calculate the maximum amount of trailing zeros found on the unscaled
%% value of the bigdec.
%%
%% Use tail recursion to validate how many trailing zeros can be found, and
%% returns the maximum amount of trailing zeros. If the value of bigdec is zero
%% than we cant calculate the amount of trailing zeros, therefore we return the
%% atom infinity, because the exp can be changed to zero as well without losing
%% numeric precision.
%% @end
%%-----------------------------------------------------------------------------
-spec howmany_trailing_zeros(Number :: integer(), Acc :: integer()) ->
                             Result :: non_neg_integer() | infinity.
howmany_trailing_zeros(Value, Amount) when Value =/= 0,
                                           is_integer(Value),
                                           is_integer(Amount) ->
  case Value rem (hlp_pow(10, Amount + 1)) == 0 of
    true  -> howmany_trailing_zeros(Value, Amount + 1);
    false -> Amount
  end;
howmany_trailing_zeros(Value, _) when Value == 0 -> infinity.


%%=============================================================================
%% Internal Functions - Utilities
%%
%% This section provides utility functions for common helper patterns that does
%% not have direct connection with #bigdec{}.
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Calculates exponentiation by squaring with big integers.
%%
%% Execute exponentiation of numbers, using recursive call of erlang pure ints,
%% but it does not do fractional exponent, or count for negative exponent. At
%% these cases it returns error.
%% @end
%%-----------------------------------------------------------------------------
-spec hlp_pow(Number   :: integer(),
              Exponent :: non_neg_integer()) ->
              Result   :: integer().
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

neg_test() ->
  ?assertEqual(    #bigdec{sign = 0, value = 120394823, exp = 450},
               neg(#bigdec{sign = 1, value = 120394823, exp = 450})),
  ?assertEqual(    #bigdec{sign = 1, value = 35, exp = 10},
               neg(#bigdec{sign = 0, value = 35, exp = 10})),
  ?assertEqual(    #bigdec{sign =    0, value = 1, exp = 4},
               neg(#bigdec{sign = '-1', value = 1, exp = 4})).

strip_zeros_test() ->
  ?assertEqual(            #bigdec{sign = 0, value =        34078, exp =  5},
               strip_zeros(#bigdec{sign = 0, value = 340780000000, exp = 12})),
  ?assertEqual(            #bigdec{sign = 1, value =    103, exp = 0},
               strip_zeros(#bigdec{sign = 1, value = 103000, exp = 3})),
  ?assertEqual(            #bigdec{sign = 0, value = 5001, exp = 12},
               strip_zeros(#bigdec{sign = 0, value = 5001, exp = 12})).

is_zero_test() ->
  ?assertEqual(true,  is_zero(zero())),
  ?assertEqual(true,  is_zero(#bigdec{sign = 1, value = 0, exp = 3})),
  ?assertEqual(false, is_zero(one())).

is_one_test() ->
  ?assertEqual(true,  is_one(one())),
  ?assertEqual(true,  is_one(#bigdec{sign = 0, value = 1000, exp = 3})),
  ?assertEqual(false, is_one(#bigdec{sign = 0, value = 1000, exp = 2})),
  ?assertEqual(false, is_one(#bigdec{sign = 1, value = 1000, exp = 3})),
  ?assertEqual(false, is_one(zero())).

is_ten_test() ->
  ?assertEqual(true,  is_ten(ten())),
  ?assertEqual(true,  is_ten(#bigdec{sign = 0, value = 1000, exp = 2})),
  ?assertEqual(false, is_ten(#bigdec{sign = 0, value = 1000, exp = 1})),
  ?assertEqual(false, is_ten(#bigdec{sign = 1, value = 1000, exp = 2})),
  ?assertEqual(false, is_ten(one())).

contains_integer_test() ->
  ?assertEqual(true,  contains_integer(#bigdec{sign = 1, value =   3, exp = 0})),
  ?assertEqual(false, contains_integer(#bigdec{sign = 1, value =   3, exp = 3})),
  ?assertEqual(true,  contains_integer(#bigdec{sign = 0, value = 200, exp = 2})),
  ?assertEqual(true,  contains_integer( one())),
  ?assertEqual(true,  contains_integer(zero())),
  ?assertEqual(true,  contains_integer( ten())).

match_exp_test() ->
  ?assertEqual({3, #bigdec{value =  10000, exp=3},
                   #bigdec{value =      5, exp=3}}, match_exp(ten(),
                                                              #bigdec{value =   5, exp = 3})),
  ?assertEqual({5, #bigdec{value = 100000, exp=5},
                   #bigdec{value =      5, exp=5}}, match_exp(one(),
                                                              #bigdec{value = 500, exp = 7})),
  ?assertEqual({1, #bigdec{value =     20, exp=1},
                   #bigdec{value =      1, exp=1}}, match_exp(#bigdec{value =   2, exp = 0},
                                                              #bigdec{value =   1, exp = 1})).

exponent_val_test() ->
  ?assertEqual(10,  exponent_val(#bigdec{sign = 0, value = 0, exp =  10})),
  ?assertEqual(302, exponent_val(#bigdec{sign = 0, value = 0, exp = 302})).

unscaled_val_test() ->
  ?assertEqual(9233498374981274897489178971489374,
               unscaled_val(#bigdec{sign = 0,
                                    value = 9233498374981274897489178971489374,
                                    exp = 10})),
  ?assertEqual(9230000000000000000000000000000000,
               unscaled_val(#bigdec{sign = 0,
                                    value = 9230000000000000000000000000000000,
                                    exp = 10})).

sign_val_test() ->
  ?assertEqual(positive, sign_val(#bigdec{sign =  0, value = 0, exp = 0})),
  ?assertEqual(positive, sign_val(#bigdec{sign =  0, value = 1, exp = 0})),
  ?assertEqual(negative, sign_val(#bigdec{sign =  1, value = 1, exp = 0})),
  ?assertEqual(invalid,  sign_val(#bigdec{sign = -1, value = 1, exp = 0})),
  ?assertEqual(invalid,  sign_val(#bigdec{sign = '0', value = 0, exp = 0})).

precision_val_test() ->
  ?assertEqual(10, precision_val(#bigdec{sign = 0, value = 1483495837, exp = 5})),
  ?assertEqual( 3, precision_val(#bigdec{sign = 0, value =        222, exp = 5})),
  ?assertEqual( 1, precision_val(#bigdec{sign = 0, value =          0, exp = 0})).

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
  ?assertEqual({true,  3}, has_trailing_zeros(#bigdec{value =       1000, exp = 10})),
  ?assertEqual({true,  3}, has_trailing_zeros(#bigdec{value =      -1000, exp = 10})),
  ?assertEqual({false, 0}, has_trailing_zeros(#bigdec{value = 4392345897, exp = 50})).

pow_test() ->
  ?assertEqual(25,                              hlp_pow(-5,   2)),
  ?assertEqual(4261655511456885005249781170176, hlp_pow(34,  20)),
  ?assertError(function_clause,                 hlp_pow(10, 0.5)),
  ?assertError(function_clause,                 hlp_pow(10,  -2)).

-endif.