%%%--------------------------------------------------------------------------------------------------------------------
%%% BigDec Library Arithmetic
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
%%% @doc Arbitrary Precision Decimal Arithmetic.
%%%
%%% Defines arithmetic operations among bigdec numbers.
%%% @end
%%%
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

%% Arithmetic operations
-export([add/2, minus/2, mult/2]).

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
%%% div(  #bigdec{}, #bigdec{})  -> #bigdec{}
%%% pow(  #bigdec{}, integer)    -> #bigdec{}
%%% rem(  #bigdec{}, #bigdec{})  -> #bigdec{}
%%%
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

-endif.