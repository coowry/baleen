-module(baleen).

%% API exports
-export([validate/2]).

-export([fail/0, success/0]).

-export([chain/1, all/1]).

-type validator_result(R) :: {ok, R} | {error, binary()} | boolean().

-type validator(A,B) :: fun((A) -> validator_result(B)).

%%====================================================================
%% API functions
%%====================================================================
-spec validate(validator(A,B), A) -> validator_result(B).
validate(Validator, Data) -> Validator(Data).

-spec fail() -> validator(_,_).
fail() -> fun(X) ->
              Error_Chars = io_lib:format("Fail to validate term \"~w\"", [X]),
              Error = unicode:characters_to_binary(Error_Chars),
              {error, Error}
          end.

-spec success() -> validator(_,_).
success() ->
    fun(X) -> {ok, X} end.

-spec chain(nonempty_list(validator(A,A))) -> validator(A,A).
chain(_Validators) -> success().

-spec all(list(validator(A,B))) -> validator(A,B).
all(_Validators) -> success().

%%====================================================================
%% Internal functions
%%====================================================================
