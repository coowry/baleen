%% @author Ángel Herranz, Manuel Cherep, Miguel Emilio Ruiz
%%
%% @copyright 2017 Coowry Ltd.
%%
%% @doc Data validation in Erlang.
%%
%% Validators of type {@type validator(A,B)} are functions that accept
%% terms of type {@type A} and returns a validation result of type
%% {@type result(B)}.
%%
%% @TODO complete the documentation
-module(baleen).

%% AH: we have some unit tests embedded in the implementation.
-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([ok_result/1, error_result/1, is_ok/1, is_error/1]).
-export([validate/2]).
-export([predicate/1]).
-export([invalid/0, valid/0]).
-export([integer_from_string/0]).
-export([compose/2, compose/1, all/1, member/1]).

%%====================================================================
%% Types
%%====================================================================

-type result(R) :: {ok, R} | {error, binary()}.

-type validator(A,B) :: fun((A) -> result(B)).

-type predicate(A) :: fun((A) -> boolean()).

%%====================================================================
%% API functions
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec is_ok(result(A)) -> boolean() when A :: term().
is_ok({ok, _}) -> true;
is_ok({error, Message}) when is_binary(Message) -> false;
is_ok(_) -> throw(badarg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec is_error(result(A)) -> boolean() when A :: term().
is_error({error, Message}) when is_binary(Message) -> true;
is_error({error, _}) -> throw(badarg);
is_error({ok, _}) -> false;
is_error(_) -> throw(badarg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ok_result(result(A)) -> A.
%% @doc Returns `X' from `{ok, X}'.
%% @throws badarg of parameter is not a tuple `{ok, _}'.
ok_result({ok, X}) -> X;
ok_result(_) -> throw(badarg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec error_result(result(A)) -> binary() when A :: term().
%% @doc Returns `Message' in `{error, Message}'.
%% @throws badarg of parameter is not a tuple `{error, _}'.
error_result({error, Message}) -> Message;
error_result(_) -> throw(badarg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec validate(validator(A,B), A) -> result(B).
%% @doc Validates data with a validator. `X' is the term to be
%% validated with validator `V'.
validate(V, X) -> V(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec invalid() -> validator(_,_).
invalid() -> fun(X) ->
                 {error, format("Invalid term \"~w\"", [X])}
             end.

invalid_simple_test_() ->
  [ ?_assertMatch({error, <<"Invalid term",_/binary>>},
                  validate(invalid(), "Hola"))
  , ?_assertNotMatch({ok, _},
                     validate(invalid(), 42)) ].

invalid_test_() ->
  Values = [true, null, undefined, "Hola", <<"">>, <<"Hola">>, 1, 0 , -1],
  [ ?_assertMatch({error, <<"Invalid term",_/binary>>},
                  validate(invalid(), Value))
    || Value <- Values ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec valid() -> validator(_,_).
valid() ->
    fun(X) -> {ok, X} end.

valid_test_() ->
  Values = [true, null, undefined, "Hola", <<"">>, <<"Hola">>, 1, 0 , -1],
  [ ?_assertMatch({ok, Value},
                  validate(valid(), Value))
    || Value <- Values ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec integer_from_string() -> validator(string(), integer()).
integer_from_string() ->
  fun(Value) ->
      case io_lib:fread("~d",Value) of
        {ok, [Integer], []} -> {ok, Integer};
        _ -> {error, format("\"~w\" is not an integer", [Value])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec compose(nonempty_list(validator(A,A))) -> validator(A,A).
compose(Validators) -> lists:foldr(fun compose/2, valid(), Validators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec all(list(validator(A,B))) -> validator(A,B).
all(_Validators) -> valid().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec member(list(A)) -> validator(A,A).
member(L) ->
  fun(T) ->
       case lists:member(T, L) of
         true ->
           {ok, T};
         false ->
           {error, format("\"~w\" is not member of \"~w\"",[T, L])}
       end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec regexp(iodata()) -> validator(String, String)
                          when String :: iodata() | unicode:charlist().
regexp(Regexp) ->
  {ok, RE} = re:compile(Regexp),
  fun (String) ->
      case re:run(String, RE) of
        nomatch -> {error, format("\"~s\" does not match regexp \"~w\"", [String, Regexp])};
        _ -> {ok, String}
      end
  end.

regexp_test_() ->
  Email_Regexp = "^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$",
  Emails = ["aherranz@gmail.com", <<"angel.herranz@coowry.com">>],
  No_Emails = ["1", "aherranz", <<"angel.herranz.coowry.com">>],
  Email_Validator = regexp(Email_Regexp),
  [?_assertEqual({ok, Email}, validate(Email_Validator, Email))
   || Email <- Emails ]
  ++
  [?_assertMatch({error, _}, validate(Email_Validator, No_Email))
   || No_Email <- No_Emails ].

%%====================================================================
%% Internal functions
%%====================================================================
-spec format(io:format(), [term()]) -> binary().
format(Format, Terms) ->
  Message = io_lib:format(Format, Terms),
  unicode:characters_to_binary(Message).
