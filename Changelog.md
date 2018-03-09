# Changelog


## Next release

## v1.0.2

### Added:
```erlang
-export([between/2, between_open_start/2, between_open_end/2, between_open/2]).
-export([to_string/0, to_binary/0]).
```

## v1.0.1

### Added:
-new types:
```erlang
-export_type([validator/2, result/1, predicate/1, str/0, val_map_validator/3, val_map_result/2]).
```

## v1.0.0

### Changed:
- list_of error message improved: invalid term and message progressed.
- map_of error message improved: invalid {key=>value} and message progressed.
- tuple_of error message improved: invalid term and message progressed.

### Removed:
```erlang
all/1
```


## v0.2.3

### Added:
- All functions documented with EDoc.


## v0.2.2

### Changed:
- EUnit tests moved from src directory to test directory.


## v0.2.1

### Changed:
- Function map_of/1 improved.
- Internal function compose_map/2 improved.


## v0.2.0

### Changed:
- Function max_length/1 fixed.
- Function validator/1 fixed.

### Added:
```erlang
%% Type casting validators
-export([to_float/1]).
%% Validator constructors
-export([val_map/2]).
```
 
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
