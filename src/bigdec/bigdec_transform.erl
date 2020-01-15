%%%----------------------------------------------------------------------------
%%% BigDec Library Transform
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
%%% @doc Arbitrary Precision Decimal Transform functions.
%%%
%%% Defines modifications and transformations of bigdec.
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(bigdec_transform).
%% @headerfile ["bigdec.hrl"]

%%=============================================================================
%% Data Structures
%%=============================================================================

-include("bigdec.hrl").

%%=============================================================================
%% Module setup
%%=============================================================================

%% Transform
-export([neg/1, strip_zeros/1]).

%%=============================================================================
%% EUnit setup
%%=============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Negate bigdec changing its sign from 0 to 1 or vice-versa.
%%
%% If sign defined in bigdec is invalid (different from 0 or 1), we return a
%% correction bigdec defining sign as 0 (positive of zero).
%% @end
%%-----------------------------------------------------------------------------
-spec neg(Number :: bigdec:bigdec()) -> Result :: bigdec:bigdec().
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
-spec strip_zeros(Number :: bigdec:bigdec()) -> Result :: bigdec:bigdec().
strip_zeros(Num = #bigdec{value = Value, exp = Exp}) ->
  case bigdec_analysis:has_trailing_zeros(Num) of
    {false,     0} -> Num;
    {true, Amount} -> Num#bigdec{value = Value div (bigdec_common:hlp_pow(10, Amount)),
                                 exp   = Exp - Amount}
  end.


%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).

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

-endif.