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
-module(baleen).

%% API exports

%% Types
-export_type([validator/2, result/1, predicate/1, str/0, val_map_validator/3, val_map_result/2]).

%% Main validation function
-export([validate/2]).

%% Functions for user validator injections
-export([validator/1, predicate/1]).

%% Validator composition
-export([compose/2, compose/1, any/1]).

%% Validator constructors

%% Basic validators
-export([invalid/0, valid/0]).
-export([member/1]).
-export([literal/1]).
-export([regex/1]).
-export([max_length/1]).
-export([transform/1]).

%% Type casting validators
-export([to_integer/0]).
-export([to_atom/0]).
-export([to_float/0]).

%% Validator "lifters"
-export([list_of/1]).
-export([map_of/2]).
-export([tuple_of/1]).

%% Reqopt
-export([val_map/2]).

%%====================================================================
%% Types
%%====================================================================

-type result(R) :: {ok, R} | {error, binary()}.

-type predicate(A) :: fun((A) -> boolean()).

-type str() :: string() | binary().

-opaque validator(A,B) :: fun((A) -> result(B)).

-type val_map_validator(K, A, B) :: #{K => {optional | required, validator(A, B)}}.

-type val_map_result(K, B) :: #{valid => #{K => B},
				nonvalid => #{K => binary()},
				unexpected => [K],
				missing => [K]}.

%%====================================================================
%% Error messages
%%====================================================================
-define(INVALID_FLOAT(V), format("\"~p\" is not a float", [V])).

%%====================================================================
%% API functions
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec validate(validator(A,B), A) -> result(B).
%% @doc Validates data with a validator. `X' is the term to be
%% validated with validator `V'.
validate(V, X) -> V(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec predicate(predicate(A)) -> validator(A,A).
%% @doc Returns a validator given a predicate. When validating `X'
%% with a predicate `P', if `P(X)' holds then `{ok, X}' is
%% returned. Otherwise, `{error, <<"Improper term X">>}' is
%% returned.
predicate(P) ->
  fun(X) ->
      case P(X) of
	true ->
	  {ok, X};
	false ->
	  {error, format("Improper term ~p", [X])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec validator(fun((A) -> result(B))) -> validator(A,B).
%% @doc Returns a validator given a user defined function that
%% validates.
validator(V) -> V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec invalid() -> validator(_,_).
%% @doc Returns a validator that always fails.
invalid() -> fun(X) ->
		 {error, format("Invalid term ~p", [X])}
             end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec valid() -> validator(_,_).
%% @doc Returns a validator that always validates.
valid() ->
  fun(X) -> {ok, X} end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec to_integer() -> validator(str(), integer()).
%% @doc Returns a validator that takes a `Value' and tries to
%% cast to integer. If the cast success, `{ok, Integer}' is returned,
%% otherwise, `{error, <<"Value is not an integer">>}' is returned.
to_integer() ->
  fun(Value) when is_binary(Value)->
      try erlang:binary_to_integer(Value) of
	  Integer -> {ok, Integer}
      catch
	_:_ -> {error, format("~p is not an integer", [Value])}
      end;
     (Value) ->
      case io_lib:fread("~d",Value) of
	{ok, [Integer], []} -> {ok, Integer};
	_ -> {error, format("~p is not an integer", [Value])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec compose(validator(A, B), validator(B, C)) -> validator(A, C).
%% @doc Returns a validator that is a composition of two validators.
compose(V1, V2) ->
  fun (X1) ->
      case validate(V1, X1) of
	{ok, X2} ->
	  validate(V2, X2);
	Error ->
	  Error
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec compose(nonempty_list(validator(A,A))) -> validator(A,A).
%% @doc Returns a validator that is a composition of a list of validators.
compose(Validators) -> lists:foldr(fun compose/2, valid(), Validators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec any(list(validator(A, B))) -> validator(A, B).
%% @doc Returns only one validator of a list of validators that matches.
any([]) -> invalid();
any([V|Vs]) -> 
  fun(T) ->
      case validate(V,T) of
	{ok, X1} -> 
	  {ok, X1};
	{error, _Error1} ->
	  case validate(any(Vs), T) of
	    {ok, X2} ->
	      {ok, X2};
	    {error, _Error2} -> 
	      {error, format("There isn't any valid", [])}
	  end
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec member(list(A)) -> validator(A, A).
%% @doc Returns a validator that matches only if the input is member
%% of `L'.
member(L) ->
  fun(T) ->
      case lists:member(T, L) of
	true ->
	  {ok, T};
	false ->
	  {error, format("~p is not member of ~p", [T, L])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec literal(A) -> validator(A, A).
%% @doc Returns a validator that matches only if the input is equals
%% to `Term'.
literal(Term) ->
  fun(T) ->
      case Term =:= T of
	true -> {ok, Term};
	false -> {error, format("~p and ~p do not match", [Term, T])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec regex(String) -> validator(String, String) when String :: str().
%% @doc Returns a validator that validates and `String' if matches 
%% a regular expression given.
regex(RegularExpression) ->
  %% Let's start with the compilation of the regular expression
  case re:compile(RegularExpression) of
    {ok, MP} ->
      fun(T) ->
	  case re:run(T, MP) of
	    {match, [{0,0}]} ->
	      {error, format("~p is not matching the regular expression ~p", [T, RegularExpression])};
	    {match, _Captured} ->
	      {ok, T};
	    nomatch ->
	      {error, format("~p is not matching the regular expression ~p", [T, RegularExpression])}
	  end
      end;
    {error, _ErrSpec} ->
      throw(badarg)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec max_length(non_neg_integer()) -> validator(S, S) when S :: iodata().
%% @doc Returns a validator that validates an input only if its length
%% is less or equal than the integer is specified.
max_length(I) -> 
  fun(S) ->
      case iolist_size(S) > I of
	true -> {error, format("The size of ~p is longer than ~p", [S, I])};
	false -> {ok, S}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec to_atom() -> validator(str(), atom()).
%% @doc Returns a validator that takes a `T' and tries to
%% cast to atom. If the cast success, `{ok, Atom}' is returned,
%% otherwise, `{error, <<"T is not a valid binary">>}' or
%% `{error, <<"T is not a valid string">>}' is returned.
to_atom() ->
  fun(T) when is_binary(T) ->
      try binary_to_atom(T, utf8) of
	  Atom -> {ok, Atom}
      catch
	_:_ -> {error, format("\"~p\" is not a valid binary", [T])}
      end;
     (T) ->
      try list_to_existing_atom(T) of
	  Atom -> {ok, Atom}
      catch
	_:_ -> {error, format("~p is not a valid string", [T])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec list_of(validator(A,B)) -> validator(list(A), list(B)).
%% @doc Returns a validator that matches a list.
list_of(V) ->
  fun(L) ->
      Results = [{Term, validate(V,Term)} || Term <- L],
      Errors = [{Term, Msg} || {Term, {error, Msg}} <- Results],
      case Errors of
	[] -> {ok , [Value || {_, {ok, Value}} <- Results]};
	[ {Term, Msg} | _] -> {error, format("Error in element ~p: ~s", [Term, Msg])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec map_of(validator(K1, K2), validator(V1, V2))
            -> validator(#{K1 => V1}, #{K2 => V2}).
%% @doc Returns a validator that validates a map, whose Keys are
%% validated by `VK' and Values are validated by `VV'. 
map_of(VK, VV) ->
  fun(Map) ->
      Keys = maps:keys(Map),
      Values = maps:values(Map),
      Results = [{{K,V},{validate(VK, K), validate(VV, V)}} || K <- Keys, V <- Values, maps:get(K, Map) =:= V],
      KeysErrors = [{K, Msg} || {{K, _},{{error, Msg}, _}} <- Results],
      ValuesErrors = [{V, Msg} || {{_, V}, {_, {error, Msg}}} <- Results],
      case KeysErrors of
	[] -> 
	  case ValuesErrors of
	    [] ->
	      {ok, maps:from_list([{ValueK, ValueV} || {{_,_}, {{ok, ValueK}, {ok, ValueV}}} <- Results])};
	    [{V, Msg} | _] -> {error, format("Error in value ~p: ~s", [V, Msg])}
	  end;
	[{K, Msg} | _] -> {error, format("Error in key ~p: ~s", [K, Msg])}
      end
  end.	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec tuple_of(validator(A, B)) -> validator({A}, {B}).
%% @doc Returns a validator that matches a tuple.
tuple_of(V) -> 
  fun(Tuple) ->
      TupleList = tuple_to_list(Tuple),
      Results = [{T, validate(V, T)} || T <- TupleList],
      Errors = [{T, Msg} || {T, {error, Msg}} <- Results],
      case Errors of
	[] -> {ok, list_to_tuple([Value || {_,{ok, Value}} <- Results])};
	[{T, Msg} | _] -> {error, format("Error in ~p: ~s", [T, Msg])}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec transform(fun((A) -> B)) -> validator(A,B).
%% @doc Returns a validator that always success and applies `F'.
transform(F) ->
  fun(T) ->
      {ok, F(T)}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec to_float() -> validator(str(), float()).
%% @doc Returns a validator that takes a `Value' and tries to
%% cast to float. If the cast success, `{ok, Float}' is returned,
%% otherwise, `{error, <<"Value is not a float">>}' is returned.
to_float() ->
  fun(Value) when is_binary(Value) ->
      try erlang:binary_to_float(Value) of
	  Float -> {ok, Float}
      catch
	_:_ -> {error, ?INVALID_FLOAT(Value)}
      end;
     (Value) ->
      case io_lib:fread("~f", Value) of
	{ok, [Float], []} -> {ok, Float};
	_ ->  {error, ?INVALID_FLOAT(Value)}
      end
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec val_map(val_map_validator(K, A, B),
	      #{K => A}) ->
		 val_map_result(K, B).
%% @doc Returns a map of the keys that matches, the keys that doesn't,
%% the keys that are missing and the unexpected keys, and their
%% corresponding values.
val_map(Validator, Map) -> 
  KeysMap = maps:keys(Map),
  Unexpected = KeysMap -- maps:keys(Validator),
  Missing = [K || {K, {OptReq, _}} <- maps:to_list(Validator), OptReq =:= required] -- KeysMap,
  ToValidate = KeysMap -- Unexpected,
  MapToValidate = lists:foldl(fun(K, AccMap) -> AccMap #{K => maps:get(K, Map)} end,
			      #{},
			      ToValidate),
  {Valids, Invalids} = maps:fold(fun(K, V, {AccValids, AccInvalids}) ->
				     {_, Val} = maps:get(K, Validator),
				     case validate(Val, V) of
				       {ok, R} -> {AccValids #{K => R}, AccInvalids}; % Add to Valid map
				       {error, Msg} -> {AccValids, AccInvalids #{K => Msg}} % Add to Invalid map
				     end
				 end, {#{},#{}}, MapToValidate),
  #{valid => Valids,
    nonvalid => Invalids,
    missing => Missing,
    unexpected => Unexpected}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%====================================================================
%% Internal functions
%%====================================================================
-spec format(io:format(), [term()]) -> binary().
format(Format, Terms) ->
  Message = io_lib:format(Format, Terms),
  unicode:characters_to_binary(Message).
