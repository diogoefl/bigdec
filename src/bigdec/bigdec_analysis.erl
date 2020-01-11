%%%----------------------------------------------------------------------------
%%% BigDec Library Analysis
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
%%% @doc Arbitrary Precision Decimal Analysis.
%%%
%%% Defines group of functions for analysis of bigdec numbers.
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(bigdec_analysis).
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

%% Analysis
-export([exponent_val/1, unscaled_val/1, sign_val/1, precision_val/1]).

%% Internal app functions
-export([has_trailing_zeros/1]).

%%=============================================================================
%% EUnit setup
%%=============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
  case Value rem (bigdec_common:hlp_pow(10, Amount + 1)) == 0 of
    true  -> howmany_trailing_zeros(Value, Amount + 1);
    false -> Amount
  end;
howmany_trailing_zeros(Value, _) when Value == 0 -> infinity.

%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).


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

has_trailing_zeros_test() ->
  ?assertEqual({true,  3}, has_trailing_zeros(#bigdec{value =       1000, exp = 10})),
  ?assertEqual({true,  3}, has_trailing_zeros(#bigdec{value =      -1000, exp = 10})),
  ?assertEqual({false, 0}, has_trailing_zeros(#bigdec{value = 4392345897, exp = 50})).

-endif.