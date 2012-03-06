% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mongrel_types).

%% Exported Functions
-export([binary/1,
		 uuid/1,
		 md5/1]).

%% External functions
binary(X) when is_binary(X) ->
	{bin, bin, X}.

uuid(X) when is_binary(X) ->
	{bin, uuid, X}.

md5(X) when is_binary(X) ->
	{bin, md5, X}.
