%% @author Ángel Herranz, Manuel Cherep, Miguel Emilio Ruiz
%%
%% @copyright 2017 Coowry Ltd.
%%
%% @doc Data validation in Erlang.
%%
%% Validators of type {@type validator(A,B)} are functions that accept
%% terms of type {@type A} and returns a validation result of type
%% {@type validator_result(B)}.
%%
%% @TODO complete the documentation
-module(baleen).

%% API exports
-export([validate/2]).
-export([predicate/1]).
-export([invalid/0, valid/0]).
-export([integer_from_string/0]).
-export([compose/2]).
-export([chain/1, all/1]).

%%====================================================================
%% Types
%%====================================================================

-type validator_result(R) :: {ok, R} | {error, binary()}.

-type validator(A,B) :: fun((A) -> validator_result(B)).

-type predicate(A) :: fun((A) -> boolean()).

%%====================================================================
%% API functions
%%====================================================================

-spec validate(validator(A,B), A) -> validator_result(B).
%% @doc Validates data with a validator. `X' is the term to be
%% validated with validator `V'.
validate(V, X) -> V(X).

-spec predicate(predicate(A)) -> validator(A,A) when A :: term().
%% @doc Returns a validator given a predicate. When validating `X'
%% with a predicate `P', if `P(X)' holds then `{ok, X}' is
%% returned. Otherwise, `{error, <<"Improper term  \"X\"">>}' is
%% returned.
predicate(P) ->
  fun(X) ->
      case P(X) of
        true ->
          {ok, X};
        false ->
          {error, format("Improper term \"~w\"", [X])}
      end
  end.

-spec invalid() -> validator(_,_).
invalid() -> fun(X) ->
                 {error, format("Invalid term \"~w\"", [X])}
             end.

-spec valid() -> validator(_,_).
valid() ->
    fun(X) -> {ok, X} end.

-spec integer_from_string() -> validator(string(), integer()).
integer_from_string() ->
  fun(Value) ->
      case io_lib:fread("~d",Value) of
        {ok, [Integer], []} -> {ok, Integer};
        _ -> {error, format("\"~w\" is not an integer", [Value])}
      end
  end.

-spec chain(nonempty_list(validator(A,A))) -> validator(A,A).
chain(Validators) -> lists:foldr(fun compose/2, valid(), Validators).

-spec compose(validator(A,B), validator(B,C)) -> validator(A,C).
compose(V1, V2) ->
  fun (X1) ->
      case V1(X1) of
        {ok, X2} ->
          V2(X2);
        Error ->
          Error
      end
  end.

-spec all(list(validator(A,B))) -> validator(A,B).
all(_Validators) -> valid().

%%====================================================================
%% Internal functions
%%====================================================================
-spec format(io:format(), [term()]) -> binary().
format(Format, Terms) ->
  Message = io_lib:format(Format, Terms),
  unicode:characters_to_binary(Message).
