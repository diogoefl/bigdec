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
%%% @doc Arbitrary Precision Decimal comparison utilities.
%%%
%%% Defines group of functions that compares bigdec numbers.
%%% @author diogoefl
%%% @copyright (diogoefl) 2020. All Rights Reserved.
%%% @end
%%%--------------------------------------------------------------------------------------------------------------------
-module(bigdec_comp).
%% @headerfile ["bigdec.hrl"]

%%%====================================================================================================================
%%% Data Structures
%%%====================================================================================================================
-include("bigdec.hrl").

%%%====================================================================================================================
%%% Module setup
%%%====================================================================================================================
-export([min/2, max/2, compare/2, is_smaller_or_equal/2, is_greater_or_equal/2, is_equal/2, is_smaller/2, is_greater/2,
         is_zero/1, is_one/1, is_ten/1, contains_integer/1, match_exp/2]).

%%%====================================================================================================================
%%% EUnit setup
%%%====================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%====================================================================================================================
%%% Library public functions - Comparison
%%%====================================================================================================================

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Returns which of the two BigDec Numbers is the greatest in value, and if they are both equals, return first
%% argument.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec max(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> MaxValue :: bigdec:bigdec().
max(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  case is_greater_or_equal(Num1, Num2) of
    true  -> Num1;
    false -> Num2
  end.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Returns which of the two BigDec Numbers is the smallest in value, and if they are both equals, return first
%% argument.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec min(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> MinValue :: bigdec:bigdec().
min(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  case is_smaller_or_equal(Num1, Num2) of
    true  -> Num1;
    false -> Num2
  end.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Compares BigDec Number1 with BigDec Number2 and returns atom defining if Number1 is equal, greater or smaller
%% than Number2.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec compare(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: equal | greater | smaller.
compare(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  case is_equal(Num1, Num2) of
    true  -> equal;
    false -> case is_smaller(Num1, Num2) of
               true  -> smaller;
               false -> greater
             end
  end.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Verifies if both bigdec numbers have equal numerical values.
%%
%% Checks if bigdec can be precisely represent the same value as the other, and if they don't match em exponent value,
%% recheck by matching exponents.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_equal(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: true | false.
%% They don't match in exponent - recalculate matching exponents for comparison
is_equal(Num1 = #bigdec{exp = Exp1}, Num2 = #bigdec{exp = Exp2}) when Exp1 =/= Exp2 ->
  {_, MatchingNum1, MatchingNum2} = match_exp(Num1, Num2),
  is_equal(MatchingNum1, MatchingNum2);

%% Both have the same sign, value and exponent
is_equal(#bigdec{sign = Sign, value = Value, exp = Exp}, #bigdec{sign = Sign, value = Value, exp = Exp}) -> true;

%% Any other case when matching exponents means they are not equals
is_equal(#bigdec{exp = Exp}, #bigdec{exp = Exp}) -> false.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Verifies if first argument is smaller than second argument.
%%
%% Checks if bigdec Number1 represents a numeric value smaller than Number2.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_smaller(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: true | false.
%% They don't match in exponent - recalculate matching exponents for comparison
is_smaller(Num1 = #bigdec{exp = Exp1}, Num2 = #bigdec{exp = Exp2}) when Exp1 =/= Exp2 ->
  {_, MatchingNum1, MatchingNum2} = match_exp(Num1, Num2),
  is_smaller(MatchingNum1, MatchingNum2);

%% Both are positive and exponent is equal, than we can compare values
is_smaller(#bigdec{sign = 0, value = Val1, exp = Exp}, #bigdec{sign = 0, value = Val2, exp = Exp}) when Val1 < Val2 ->
  true;

%% Both are negative and exponent is equal, than we can compare values
is_smaller(#bigdec{sign = 1, value = Val1, exp = Exp}, #bigdec{sign = 1, value = Val2, exp = Exp}) when Val1 > Val2 ->
  true;

%% Num1 is negative and Num2 is positive and exponent is equal, we don't need
%% to check the values because a negative number will always be smaller
is_smaller(#bigdec{sign = 1, exp = Exp}, #bigdec{sign = 0, exp = Exp}) -> true;

%% Any other case when matching exponents means Num1 is not smaller than Num2
is_smaller(#bigdec{exp = Exp}, #bigdec{exp = Exp}) -> false.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Verifies if first argument is greater than second argument.
%%
%% Checks if bigdec Number1 represents a numeric value greater than Number2.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_greater(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: true | false.
%% They don't match in exponent - recalculate matching exponents for comparison
is_greater(Num1 = #bigdec{exp = Exp1}, Num2 = #bigdec{exp = Exp2}) when Exp1 =/= Exp2 ->
  {_, MatchingNum1, MatchingNum2} = match_exp(Num1, Num2),
  is_greater(MatchingNum1, MatchingNum2);

%% Both are positive and exponent is equal, than we can compare values
is_greater(#bigdec{sign = 0, value = Val1, exp = Exp}, #bigdec{sign = 0, value = Val2, exp = Exp}) when Val1 > Val2 ->
  true;

%% Both are negative and exponent is equal, than we can compare values
is_greater(#bigdec{sign = 1, value = Val1, exp = Exp}, #bigdec{sign = 1, value = Val2, exp = Exp}) when Val1 < Val2 ->
  true;

%% Num1 is positive and Num2 is negative and exponent is equal, we don't need
%% to check the values because a negative number will always be smaller
is_greater(#bigdec{sign = 0, exp = Exp}, #bigdec{sign = 1, exp = Exp}) -> true;

%% Any other case when matching exponents means Num1 is not greater than Num2
is_greater(#bigdec{exp = Exp}, #bigdec{exp = Exp}) -> false.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Validates if BigDec Number1 is smaller than or equal to BigDec Number2.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_smaller_or_equal(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: true | false.
is_smaller_or_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  is_equal(Num1, Num2) orelse is_smaller(Num1, Num2).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Validates if BigDec Number1 is greater than or equal to BigDec Number2.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_greater_or_equal(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) -> Result :: true | false.
is_greater_or_equal(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  is_equal(Num1, Num2) orelse is_greater(Num1, Num2).

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Validates if bigdec has equivalent value of zero.
%%
%% Checks if bigdec can be precisely represented as zero number, this is validated by first stripping number, and than
%% checking if value and exp are equivalent to zero.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_zero(Number :: bigdec:bigdec()) -> Result :: true | false.
is_zero(Num = #bigdec{}) ->
  %% First we strip the number for later evaluation
  #bigdec{value = Value, exp = Exp} = bigdec_transform:strip_zeros(Num),
  Value == 0 andalso Exp == 0.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Validates if bigdec has equivalent value of one.
%%
%% Checks if bigdec can be precisely represented as number one, this is validated by first stripping number, and than
%% checking if value is equal to one, sign equal to zero and exp equal to zero.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_one(Number :: bigdec:bigdec()) -> Result :: true | false.
is_one(Num = #bigdec{}) ->
  %% First we strip the number for later evaluation
  #bigdec{sign = Sign, value = Value, exp = Exp} = bigdec_transform:strip_zeros(Num),
  Sign == 0 andalso Value == 1 andalso Exp == 0.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Validates if bigdec has equivalent value of ten.
%%
%% Checks if bigdec can be precisely represented as number ten, this is validated by first stripping number, and than
%% checking if value is equal to ten, sign equal to zero and exp equal to zero.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec is_ten(Number :: bigdec:bigdec()) -> Result :: true | false.
is_ten(Num = #bigdec{}) ->
  %% First we strip the number for later evaluation
  #bigdec{sign = Sign, value = Value, exp = Exp} = bigdec_transform:strip_zeros(Num),
  Sign == 0 andalso Value == 10 andalso Exp == 0.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Validates if the value contained in bigdec represents an integer.
%%
%% Checks if bigdec can be precisely represented as a simple integer, this is validated by check if exp is zero, and
%% also if the amount of trailing zeros is equivalent to the exp value. Function does not use pattern is_* to avoid
%% conflict with BIF is_integer.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec contains_integer(Number :: bigdec:bigdec()) -> Result :: true | false.
contains_integer(Num = #bigdec{exp = Exp}) ->
  case Exp of
    %% We have no decimal places in bigdec
    0 -> true;
    %% We have decimal places but we need to check trailing zeros
    _ -> case bigdec_analysis:has_trailing_zeros(Num) of
           %% The amount of trailing zeros is equivalent to decimal places
           {true, Exp} -> true;
           %% Any other condition
           _           -> false
         end
  end.

%%---------------------------------------------------------------------------------------------------------------------
%% @doc Compare two bigdec values and define what is the matching exponent.
%%
%% Calculates the common exponent among both bigdec data, but considers first stripping zeros if they exist. Returns
%% the biggest exponent to be used by other functions for calculation, because we can increase exponent without losing
%% precision of data, but not the other way around.
%% @end
%%---------------------------------------------------------------------------------------------------------------------
-spec match_exp(Number1 :: bigdec:bigdec(), Number2 :: bigdec:bigdec()) ->
                Result  :: {integer(), bigdec:bigdec(), bigdec:bigdec()}.
match_exp(Num1 = #bigdec{}, Num2 = #bigdec{}) ->
  %% Retrieve the stripped numbers for calculation
  StrippedNum1 = #bigdec{value = Value1, exp = Exp1} = bigdec_transform:strip_zeros(Num1),
  StrippedNum2 = #bigdec{value = Value2, exp = Exp2} = bigdec_transform:strip_zeros(Num2),
  %% Returns the equalized numbers matching exp value
  case Exp1 > Exp2 of
    true  -> {Exp1, StrippedNum1,
                    StrippedNum2#bigdec{value = Value2 * bigdec_common:hlp_pow(10,Exp1-Exp2),
                                        exp   = Exp1}};
    false -> {Exp2, StrippedNum1#bigdec{value = Value1 * bigdec_common:hlp_pow(10,Exp2-Exp1),
                                        exp   = Exp2},
                    StrippedNum2}
  end.

%%%====================================================================================================================
%%% EUnit Tests
%%%====================================================================================================================
-ifdef(TEST).

min_test() ->
  ?assertEqual(           #bigdec{sign = 0, value = 5, exp = 2},
               bigdec:min(#bigdec{sign = 0, value = 10, exp = 1},
                          #bigdec{sign = 0, value =  5, exp = 2})).

max_test() ->
  ?assertEqual(           #bigdec{sign = 0, value = 10, exp = 1},
               bigdec:max(#bigdec{sign = 0, value = 10, exp = 1},
                          #bigdec{sign = 0, value =  5, exp = 2})).

compare_test() ->
  %% 0.55 against 0.55 -> equal
  ?assertEqual(equal,   compare(#bigdec{sign = 0, value = 55, exp = 2},
                               #bigdec{sign = 0, value = 55, exp = 2})),
  %% 0.55 against -0.55 -> greater
  ?assertEqual(greater, compare(#bigdec{sign = 0, value = 55, exp = 2},
                                #bigdec{sign = 1, value = 55, exp = 2})),
  %% 0.550 against 0.55 -> equal
  ?assertEqual(equal,   compare(#bigdec{sign = 0, value = 550, exp = 3},
                                #bigdec{sign = 0, value =  55, exp = 2})),
  %% -0.500 against -0.50 -> equal
  ?assertEqual(equal,   compare(#bigdec{sign = 1, value = 500, exp = 3},
                                #bigdec{sign = 1, value =  50, exp = 2})),
  %% 0.055 against 0.55 -> smaller
  ?assertEqual(smaller, compare(#bigdec{sign = 0, value = 55, exp = 3},
                                #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.55 against 0.55 -> true
  ?assertEqual(smaller, compare(#bigdec{sign = 1, value = 55, exp = 2},
                                #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.0500 against -0.50 -> greater
  ?assertEqual(greater, compare(#bigdec{sign = 1, value = 500, exp = 4},
                                #bigdec{sign = 1, value =  50, exp = 2})).

is_equal_test() ->
  %% 0.55 =:= 0.55 -> true
  ?assertEqual(true,  is_equal(#bigdec{sign = 0, value = 55, exp = 2},
                               #bigdec{sign = 0, value = 55, exp = 2})),
  %% 0.55 =:= -0.55 -> false
  ?assertEqual(false, is_equal(#bigdec{sign = 0, value = 55, exp = 2},
                               #bigdec{sign = 1, value = 55, exp = 2})),
  %% 0.550 =:= 0.55 -> true
  ?assertEqual(true,  is_equal(#bigdec{sign = 0, value = 550, exp = 3},
                               #bigdec{sign = 0, value =  55, exp = 2})),
  %% -0.500 =:= -0.50 -> true
  ?assertEqual(true,  is_equal(#bigdec{sign = 1, value = 500, exp = 3},
                               #bigdec{sign = 1, value =  50, exp = 2})).

is_smaller_test() ->
  %% 0.055 < 0.55 -> true
  ?assertEqual(true,  is_smaller(#bigdec{sign = 0, value = 55, exp = 3},
                                 #bigdec{sign = 0, value = 55, exp = 2})),
  %% 0.55 < 0.55 -> false
  ?assertEqual(false, is_smaller(#bigdec{sign = 0, value = 55, exp = 2},
                                 #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.55 < 0.55 -> true
  ?assertEqual(true,  is_smaller(#bigdec{sign = 1, value = 55, exp = 2},
                                 #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.0500 < -0.50 -> false
  ?assertEqual(false, is_smaller(#bigdec{sign = 1, value = 500, exp = 4},
                                 #bigdec{sign = 1, value =  50, exp = 2})).

is_smaller_or_equal_test() ->
  %% 0.055 < 0.55 -> true
  ?assertEqual(true,  is_smaller_or_equal(#bigdec{sign = 0, value = 55, exp = 3},
                                          #bigdec{sign = 0, value = 55, exp = 2})),
  %% 0.55 < 0.55 -> false
  ?assertEqual(true,  is_smaller_or_equal(#bigdec{sign = 0, value = 55, exp = 2},
                                          #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.55 < 0.55 -> true
  ?assertEqual(true,  is_smaller_or_equal(#bigdec{sign = 1, value = 55, exp = 2},
                                          #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.0500 < -0.50 -> false
  ?assertEqual(false, is_smaller_or_equal(#bigdec{sign = 1, value = 500, exp = 4},
                                          #bigdec{sign = 1, value =  50, exp = 2})).

is_greater_test() ->
  %% 0.055 > 0.55 -> false
  ?assertEqual(false, is_greater(#bigdec{sign = 0, value = 55, exp = 3},
                                 #bigdec{sign = 0, value = 55, exp = 2})),
  %% 0.55 > 0.55 -> false
  ?assertEqual(false, is_greater(#bigdec{sign = 0, value = 55, exp = 2},
                                 #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.55 > 0.55 -> false
  ?assertEqual(false, is_greater(#bigdec{sign = 1, value = 55, exp = 2},
                                 #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.0500 > -0.50 -> true
  ?assertEqual(true,  is_greater(#bigdec{sign = 1, value = 500, exp = 4},
                                 #bigdec{sign = 1, value =  50, exp = 2})).

is_greater_or_equal_test() ->
  %% 0.055 > 0.55 -> false
  ?assertEqual(false, is_greater_or_equal(#bigdec{sign = 0, value = 55, exp = 3},
                                          #bigdec{sign = 0, value = 55, exp = 2})),
  %% 0.55 > 0.55 -> true
  ?assertEqual(true, is_greater_or_equal(#bigdec{sign = 0, value = 55, exp = 2},
                                         #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.55 > 0.55 -> false
  ?assertEqual(false, is_greater_or_equal(#bigdec{sign = 1, value = 55, exp = 2},
                                          #bigdec{sign = 0, value = 55, exp = 2})),
  %% -0.0500 > -0.50 -> true
  ?assertEqual(true,  is_greater_or_equal(#bigdec{sign = 1, value = 500, exp = 4},
                                          #bigdec{sign = 1, value =  50, exp = 2})).

is_zero_test() ->
  ?assertEqual(true,  is_zero(bigdec_const:zero())),
  ?assertEqual(true,  is_zero(#bigdec{sign = 1, value = 0, exp = 3})),
  ?assertEqual(false, is_zero(bigdec_const:one())).

is_one_test() ->
  ?assertEqual(true,  is_one(bigdec_const:one())),
  ?assertEqual(true,  is_one(#bigdec{sign = 0, value = 1000, exp = 3})),
  ?assertEqual(false, is_one(#bigdec{sign = 0, value = 1000, exp = 2})),
  ?assertEqual(false, is_one(#bigdec{sign = 1, value = 1000, exp = 3})),
  ?assertEqual(false, is_one(bigdec_const:zero())).

is_ten_test() ->
  ?assertEqual(true,  is_ten(bigdec_const:ten())),
  ?assertEqual(true,  is_ten(#bigdec{sign = 0, value = 1000, exp = 2})),
  ?assertEqual(false, is_ten(#bigdec{sign = 0, value = 1000, exp = 1})),
  ?assertEqual(false, is_ten(#bigdec{sign = 1, value = 1000, exp = 2})),
  ?assertEqual(false, is_ten(bigdec_const:one())).

contains_integer_test() ->
  ?assertEqual(true,  contains_integer(#bigdec{sign = 1, value =   3, exp = 0})),
  ?assertEqual(false, contains_integer(#bigdec{sign = 1, value =   3, exp = 3})),
  ?assertEqual(true,  contains_integer(#bigdec{sign = 0, value = 200, exp = 2})),
  ?assertEqual(true,  contains_integer(bigdec_const:one())),
  ?assertEqual(true,  contains_integer(bigdec_const:zero())),
  ?assertEqual(true,  contains_integer(bigdec_const:ten())).

match_exp_test() ->
  ?assertEqual({3, #bigdec{value =  10000, exp=3},
                   #bigdec{value =      5, exp=3}}, match_exp(bigdec_const:ten(),
                                                              #bigdec{value =   5, exp = 3})),
  ?assertEqual({5, #bigdec{value = 100000, exp=5},
                   #bigdec{value =      5, exp=5}}, match_exp(bigdec_const:one(),
                                                              #bigdec{value = 500, exp = 7})),
  ?assertEqual({1, #bigdec{value =     20, exp=1},
                   #bigdec{value =      1, exp=1}}, match_exp(#bigdec{value =   2, exp = 0},
                                                              #bigdec{value =   1, exp = 1})).

-endif.