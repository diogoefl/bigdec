%%%----------------------------------------------------------------------------
%%% BigDec Library Common
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
%%% @doc Arbitrary Precision Decimal Common utility functions and helpers.
%%%
%%% Defines common utility functions and helpers to be used internally on the
%%% library. These functions are not supposed to be used outside of bidec app.
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(bigdec_common).

%%=============================================================================
%% Module setup
%%=============================================================================

%% Constants
-export([hlp_pow/2, hlp_prepend_zeros/2]).

%%=============================================================================
%% EUnit setup
%%=============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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


%%-----------------------------------------------------------------------------
%% @doc Generates string containing leading zeros for formatting.
%%
%% Based on amount of required leading zeros generates string prepending zeros
%% to original string and returns the new string.
%% @end
%%-----------------------------------------------------------------------------
-spec hlp_prepend_zeros(NumberString :: string(), LeadingZeros :: integer()) ->
                        ResultString :: string().
hlp_prepend_zeros(IntString, LeadingZeros) when LeadingZeros > 0 ->
  lists:flatten(lists:duplicate(LeadingZeros, "0"))
  ++ IntString;

hlp_prepend_zeros(IntString, _) ->
  IntString.

%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).

pow_test() ->
  ?assertEqual(25,                              hlp_pow(-5,   2)),
  ?assertEqual(4261655511456885005249781170176, hlp_pow(34,  20)),
  ?assertError(function_clause,                 hlp_pow(10, 0.5)),
  ?assertError(function_clause,                 hlp_pow(10,  -2)).

-endif.