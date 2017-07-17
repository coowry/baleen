-module(baleen).

%% API exports
-export([fail/0, success/0]).

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

-spec chain(non_empty_list(validator(A,A))) -> validator(A,A).


-spec all()
    

%%====================================================================
%% Internal functions
%%====================================================================
