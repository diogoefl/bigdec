%%%----------------------------------------------------------------------------
%%% BigDec Library Header
%%%
%%% % StartCopyright %
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
%%% % EndCopyright %
%%%----------------------------------------------------------------------------

%%=============================================================================
%% Records and Macros
%%=============================================================================

%%-----------------------------------------------------------------------------
%% #bigdec{} defines tuple object representing a BigDec number.
%%
%% Data structure of bigdec is formed by 3 elements: sign, integer value and
%% exponent. These three elements form the definition of the number based on
%% the following formula: (Sign * -1) * IntValue * (10 ^ Exp).
%%-----------------------------------------------------------------------------
-record(bigdec, {sign  = 0 :: 0 | 1,
                 value = 0 :: non_neg_integer(),
                 exp   = 0 :: non_neg_integer()}).