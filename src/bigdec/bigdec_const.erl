%%%----------------------------------------------------------------------------
%%% BigDec Library Constants
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
%%% @doc Arbitrary Precision Decimal Constants.
%%%
%%% Defines common use of constants for bigdec numbers.
%%% @end
%%%
%%%----------------------------------------------------------------------------

-module(bigdec_const).
%% @headerfile ["bigdec.hrl"]

%%=============================================================================
%% Data Structures
%%=============================================================================

-include("bigdec.hrl").

%%=============================================================================
%% Module setup
%%=============================================================================

%% Constants
-export([one/0, zero/0, ten/0]).

%%=============================================================================
%% EUnit setup
%%=============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%=============================================================================
%% Library public functions - Constants
%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of one when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec one() -> Result :: bigdec:bigdec().
one() -> #bigdec{sign = 0, value = 1, exp = 0}.

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of zero when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec zero() -> Result :: bigdec:bigdec().
zero() -> #bigdec{sign = 0, value = 0, exp = 0}.

%%-----------------------------------------------------------------------------
%% @doc Bigdec construct regarding value of ten when exponent is 0.
%% @end
%%----------------------------------------------------------------------------
-spec ten() -> Result :: bigdec:bigdec().
ten() -> #bigdec{sign = 0, value = 10, exp = 0}.

%%=============================================================================
%% EUnit Tests
%%=============================================================================

-ifdef(TEST).

-endif.