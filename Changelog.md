# Changelog

## v0.0.1

- Initial release of baleen:

```erlang
%% API exports

%% Types
-export_type([validator/2, result/1, predicate/1, str/0]).

%% Main validation function
-export([validate/2]).

%% Functions for user validator injections
-export([validator/1, predicate/1]).

%% Validator composition
-export([compose/2, compose/1, all/1, any/1]).

%% Validator constructors
-export([invalid/0, valid/0]).
-export([member/1]).
-export([literal/1]).
-export([regex/1]).
-export([max_length/1]).
-export([list_of/1, map_of/2, tuple_of/1]).
-export([transform/1]).

%% Type casting validators
-export([to_integer/0]).
-export([to_atom/0]).
```
