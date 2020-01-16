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
%%% @doc Arbitrary Precision Decimal Arithmetic.
%%%
%%% Defines arithmetic operations among bigdec numbers.
%%% @author diogoefl
%%% @copyright (diogoefl) 2020. All Rights Reserved.
%%% @end
%%%--------------------------------------------------------------------------------------------------------------------
-module(bigdec_arith).
%% @headerfile ["bigdec.hrl"]

%%%====================================================================================================================
%%% Data Structures
%%%====================================================================================================================
-include("bigdec.hrl").

%%%====================================================================================================================
%%% Module setup
%%%====================================================================================================================
-export([add/2, minus/2, mult/2, divide/2, divide/3, divide/4]).

%%%====================================================================================================================
%%% EUnit setup
%%%====================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%====================================================================================================================
%%% Library public functions - Arithmetic
%%%
%%% Planned functions to be implemented
%%% => Arithmetic
%%% add(  #bigdec{}, #bigdec{})  -> #bigdec{} (DONE)
%%% minus(#bigdec{}, #bigdec{})  -> #bigdec{} (DONE)
%%% mult( #bigdec{}, #bigdec{})  -> #bigdec{} (DONE)
%%% div(  #bigdec{}, #bigdec{})  -> #bigdec{} (DONE)
%%% pow(  #bigdec{}, integer)    -> #bigdec{}
%%% rem(  #bigdec{}, #bigdec{})  -> #bigdec{}
%%%
%%% => Rounding Patterns
%%% round_up        => Increments the digit prior to a nonzero discarded fraction
%%% round_down      => Doesn't increment the digit prior to a discarded fraction (trunc)
%%% round_ceiling   => Round towards positive infinity - if sign is pos act as round_up, if is neg act as round_down
%%% round_floor     => Round towards negative infinity - if sign is pos act as round_down, it is neg act as round_up
%%% round_half_up   => If the discarded fraction is >= 0.5, use round_up
%%% round_half_down => If the discarded fraction is >  0.5, use round_up
%%% round_half_even => If remainder digit from discard (left digit to the discarded fraction) is even, act as
%%%                    round_half_up, otherwise use round_half_down
%%%====================================================================================================================

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Add two bigdec numbers by matching scale if necessary.
%%
%% If both numbers contain the same exp value, the resulting bigdec is the sum of values adjusted by sign with the same
%% exp. But if exp doesn't match we need to match exp of each number.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec add(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: bigdec:bigdec().
%% When both numbers are positive and exponent matches
add(#bigdec{sign = 0, value = Value1, exp = Exp}, #bigdec{sign = 0, value = Value2, exp = Exp}) ->
  bigdec_transform:strip_zeros(#bigdec{sign = 0, value = Value1 + Value2, exp = Exp});

%% When first number is + but second is -, also exponent matches and first number has bigger value than second
add(#bigdec{sign = 0, value = Value1, exp = Exp}, #bigdec{sign = 1, value = Value2, exp = Exp})
    when Value1 >= Value2 ->
  bigdec_transform:strip_zeros(#bigdec{sign = 0, value = Value1 - Value2, exp = Exp});

%% When first number is + but second is -, also exponent matches and first number has smaller value than second
add(#bigdec{sign = 0, value = Value1, exp = Exp}, #bigdec{sign = 1, value = Value2, exp = Exp})
    when Value1 < Value2 ->
  bigdec_transform:strip_zeros(#bigdec{sign = 1, value = Value2 - Value1, exp = Exp});

%% If both number are negative and exponent matches
add(#bigdec{sign = 1, value = Value1, exp = Exp}, #bigdec{sign = 1, value = Value2, exp = Exp}) ->
  bigdec_transform:strip_zeros(#bigdec{sign = 1, value = Value1 + Value2, exp = Exp});

%% If first number is negative and second number is positive we recursive call to run causes above
add(Num1 = #bigdec{sign = 1}, Num2 = #bigdec{sign = 0}) ->
  add(Num2, Num1);

%% Otherwise we don't have matching exponents and we need to equalize values first and later recur for above cases to
%% make calculation
add(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  {_, MatchedNum1, MatchedNum2} = bigdec_comp:match_exp(Num1, Num2),
  add(MatchedNum1, MatchedNum2).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Subtracts two bigdec numbers by matching scale if necessary.
%%
%% The subtraction is a wrapper for inverting sign of Num2 and proceeding with addition. This way we keep logic
%% preserved.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec minus(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: bigdec:bigdec().
minus(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  add(Num1, bigdec_transform:neg(Num2)).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Multiply two bigdec numbers and strip zeros if necessary.
%%
%% Multiplication of bigdec numbers is made by multiplying its integer values and adding exponent values. If resulting
%% bigdec contains trailing zeros, than we need to submit it for stripping zeros.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec mult(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: bigdec:bigdec().
mult(#bigdec{sign = S1, value = Val1, exp = Exp1}, #bigdec{sign = S2, value = Val2, exp = Exp2}) ->
  ResultInt  = Val1 * Val2,
  ResultExp  = Exp1 + Exp2,
  bigdec_transform:strip_zeros(#bigdec{sign  = case {S1, S2} of
                                                 {0, 0} -> 0; %% Both are positive
                                                 {0, 1} -> 1; %% Second is negative
                                                 {1, 0} -> 1; %% First is negative
                                                 {1, 1} -> 0  %% Both are negative
                                               end,
                                       value = ResultInt,
                                       exp   = ResultExp}).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Divide two bigdec numbers and strip zeros if necessary using default context.
%%
%% If no math context is given, then defaults are used as rounding mode to round_half_up and maximum exponent to 30
%% decimal places. The maximum exponent is discarded if one of the operands at the division has a bigger exponent.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec divide(Numerator   :: bigdec:bigdec(),
             Denominator :: bigdec:bigdec()) -> Result :: bigdec:bigdec().
divide(Numerator = #bigdec{}, Denominator = #bigdec{}) ->
  divide(Numerator, Denominator, round_half_up, 30).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Divide two bigdec numbers and strip zeros if necessary using partial math context.
%%
%% If only partial math context is given, then defaults are used as rounding mode to round_half_up and maximum exponent
%% to 30 decimal places.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec divide(Numerator   :: bigdec:bigdec(),
             Denominator :: bigdec:bigdec(),
             MathContext :: integer() | atom()) -> Result :: bigdec:bigdec().
divide(Numerator = #bigdec{}, Denominator = #bigdec{}, MaxExponent) when is_integer(MaxExponent) ->
  divide(Numerator, Denominator, round_half_up, MaxExponent);
divide(Numerator = #bigdec{}, Denominator = #bigdec{}, RoundingMode) when is_atom(RoundingMode) ->
  divide(Numerator, Denominator, RoundingMode, 30).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Divide two bigdec numbers and strip zeros if necessary using math context.
%%
%% Division is applied by equalizing exponents of both operands and discarding respective exponent. New exponent is
%% later applied using either MaxExponent, or the original exponent if it is bigger. The result is stripped of trailing
%% zeros for more concise result.
%% @see bigdec_common:hlp_apply_round/4. hlp_apply_round/4 at bigdec_common for information on rounding methods.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec divide(Numerator    :: bigdec:bigdec(),
             Denominator  :: bigdec:bigdec(),
             RoundingMode :: atom(),
             MaxExponent  :: integer()) -> Result :: bigdec:bigdec().
divide(Numerator = #bigdec{}, Denominator = #bigdec{}, RoundingMode, MaxExponent) ->
  %% Adjust exponents
  {_, MatchNumerator, MatchDenominator} = bigdec_comp:match_exp(Numerator, Denominator),
  %% Get the resulting sign for the operation
  ResultSign = case {MatchNumerator#bigdec.sign, MatchDenominator#bigdec.sign} of
                 {0, 0} -> 0;
                 {0, 1} -> 1;
                 {1, 0} -> 1;
                 {1, 1} -> 0
               end,
  %% Define adjustment - since the exponent is equal we can ignore it and define a new adjustment
  %% Because 1 * 10 ^ -2 / 2 * 10 ^ -2 == 1 / 2
  AdjustingExponent = case MatchDenominator#bigdec.exp > MaxExponent of
                        true  -> MatchDenominator#bigdec.exp;
                        false -> MaxExponent
                      end,
  %% Adjust numerator for new exponent
  AdjustedNumerator = MatchNumerator#bigdec.value * bigdec_common:hlp_pow(10, AdjustingExponent),
  %% Get the result by integer division
  DivisionResult = AdjustedNumerator div MatchDenominator#bigdec.value,
  %% Get the next digit that will be truncated
  TruncatedDigit = (((AdjustedNumerator rem MatchDenominator#bigdec.value) * 10) div MatchDenominator#bigdec.value),
  %% Apply rounding mode to truncated digit to define division result
  AdjustedResult = bigdec_common:hlp_apply_round(RoundingMode, ResultSign, DivisionResult, TruncatedDigit),
  %% Create a new BigDec composed by these new data
  ResultBigDec = #bigdec{sign = ResultSign, value = AdjustedResult, exp = AdjustingExponent},
  %% Return stripped result
  bigdec_transform:strip_zeros(ResultBigDec).

%%%====================================================================================================================
%%% EUnit Tests
%%%====================================================================================================================

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

mult_test() ->
  %% 1.0 * 0.500 = 0.5
  ?assertEqual(     #bigdec{sign = 0, value =   5, exp = 1},
               mult(#bigdec{sign = 0, value =  10, exp = 1},
                    #bigdec{sign = 0, value = 500, exp = 3})),
  %% 10.567 * -0.345 = -3,645615‬
  ?assertEqual(     #bigdec{sign = 1, value = 3645615, exp = 6},
               mult(#bigdec{sign = 0, value =   10567, exp = 3},
                    #bigdec{sign = 1, value =     345, exp = 3})),
  %% -5.6 * 0.0000000007 = -0.00000000392
  ?assertEqual(     #bigdec{sign = 1, value = 392, exp = 11},
               mult(#bigdec{sign = 1, value =  56, exp =  1},
                    #bigdec{sign = 0, value =   7, exp = 10})),
  %% -0.3 * -100.2 = 30.06
  ?assertEqual(     #bigdec{sign = 0, value = 3006, exp = 2},
               mult(#bigdec{sign = 1, value =    3, exp = 1},
                    #bigdec{sign = 1, value = 1002, exp = 1})).

divide_test() ->
  %% 0.4 / 0.02 = 20
  ?assertEqual(       #bigdec{sign = 0, value = 20, exp = 0},
               divide(#bigdec{sign = 0, value =  4, exp = 1},
                      #bigdec{sign = 0, value =  2, exp = 2})),
  %% 0.02 / 0.4 = 0.05
  ?assertEqual(       #bigdec{sign = 0, value = 5, exp = 2},
               divide(#bigdec{sign = 0, value = 2, exp = 2},
                      #bigdec{sign = 0, value = 4, exp = 1})),
  %% 0.00003 / -0.000000000075 = -400_000
  ?assertEqual(       #bigdec{sign = 1, value = 400000, exp =  0},
               divide(#bigdec{sign = 0, value =      3, exp =  5},
                      #bigdec{sign = 1, value =     75, exp = 12})),
  %% -3.75 / -4.683 = 0,80076873798 | TRUNC 8 - Context round_down 11
  ?assertEqual(       #bigdec{sign = 0, value = 80076873798, exp = 11},
               divide(#bigdec{sign = 1, value =         375, exp =  2},
                      #bigdec{sign = 1, value =        4683, exp =  3}, round_down, 11)),
  %% -3.75 / 4.683 = -0,80076873799 | TRUNC 8 - Context round_up 11
  ?assertEqual(       #bigdec{sign = 1, value = 80076873799, exp = 11},
               divide(#bigdec{sign = 1, value =         375, exp =  2},
                      #bigdec{sign = 0, value =        4683, exp =  3}, round_up, 11)),
  %% -3 / 7 = -0,428571428 | TRUNC 5 - Context round_half_down 9
  ?assertEqual(       #bigdec{sign = 1, value = 428571428, exp = 9},
               divide(#bigdec{sign = 1, value =         3, exp = 0},
                      #bigdec{sign = 0, value =         7, exp = 0}, round_half_down, 9)),
  %% -3 / 7 = -0,428571429 | TRUNC 5 - Context round_half_up 9
  ?assertEqual(       #bigdec{sign = 1, value = 428571429, exp = 9},
               divide(#bigdec{sign = 1, value =         3, exp = 0},
                      #bigdec{sign = 0, value =         7, exp = 0}, round_half_up, 9)),
  %% -3 / 7 = -0,428571428 | TRUNC 5 - Context round_ceiling 9
  ?assertEqual(       #bigdec{sign = 1, value = 428571428, exp = 9},
               divide(#bigdec{sign = 1, value =         3, exp = 0},
                      #bigdec{sign = 0, value =         7, exp = 0}, round_ceiling, 9)),
  %% 3 / 7 = 0,428571429 | TRUNC 5 - Context round_ceiling 9
  ?assertEqual(       #bigdec{sign = 0, value = 428571429, exp = 9},
               divide(#bigdec{sign = 0, value =         3, exp = 0},
                      #bigdec{sign = 0, value =         7, exp = 0}, round_ceiling, 9)),
  %% -3 / 7 = -0,428571428 | TRUNC 5 - Context round_half_even 9
  ?assertEqual(       #bigdec{sign = 1, value = 428571428, exp = 9},
               divide(#bigdec{sign = 1, value =         3, exp = 0},
                      #bigdec{sign = 0, value =         7, exp = 0}, round_half_even, 9)),
  %% -3 / 7 = -0,42857143 | TRUNC 8 - Context round_half_even 8
  ?assertEqual(       #bigdec{sign = 1, value = 42857143, exp = 8},
               divide(#bigdec{sign = 1, value =        3, exp = 0},
                      #bigdec{sign = 0, value =        7, exp = 0}, round_half_even, 8)),
  %% -3 / 7 = -0,4285714 | TRUNC 2 - Context round_half_even 7
  ?assertEqual(       #bigdec{sign = 1, value = 4285714, exp = 7},
               divide(#bigdec{sign = 1, value =       3, exp = 0},
                      #bigdec{sign = 0, value =       7, exp = 0}, round_half_even, 7)).

-endif.