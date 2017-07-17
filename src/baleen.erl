-module(baleen).

%% API exports
-export([fail/0, success/0]).

-export([chain/1, all/1]).

-type validator_result(R) :: {ok, R} | {error, binary()} | boolean().

-type validator(A,B) :: fun((A) -> validator_result(B)).
%%====================================================================
%% API functions
%%====================================================================
-spec fail() -> validator(_,_).
fail() ->
    fun(_) -> {error,<<"Fail validator">>} end.

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
