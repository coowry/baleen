-module(baleen).

%% API exports
-export([validate/2]).

-export([invalid/0, valid/0]).

-export([integer_from_string/0]).

-export([chain/1, all/1]).

-type validator_result(R) :: {ok, R} | {error, binary()} | boolean().

-type validator(A,B) :: fun((A) -> validator_result(B)).

%%====================================================================
%% API functions
%%====================================================================
-spec validate(validator(A,B), A) -> validator_result(B).
validate(Validator, Data) -> Validator(Data).

-spec invalid() -> validator(_,_).
invalid() -> fun(X) ->
                 {error, format("Invalid term \"~w\"", [X])}
             end.

-spec valid() -> validator(_,_).
valid() ->
    fun(X) -> {ok, X} end.

-spec integer_from_string() -> validator(string(), integer()).
integer_from_string() -> invalid().

-spec chain(nonempty_list(validator(A,A))) -> validator(A,A).
chain(_Validators) -> valid().

-spec all(list(validator(A,B))) -> validator(A,B).
all(_Validators) -> valid().

%%====================================================================
%% Internal functions
%%====================================================================
-spec format(io:format(), [term()]) -> binary().
format(Format, Terms) ->
  Message = io_lib:format(Format, Terms),
  unicode:characters_to_binary(Message).
