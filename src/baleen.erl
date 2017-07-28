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
%% @TODO functions to be studied (and brought?) from Saul (named_validator, 
%% @TODO names need to be shorter (eg. literal -> lit, atom_from_string -> string_to_atom
-module(baleen).

%% AH: we have some unit tests embedded in the implementation.
-include_lib("eunit/include/eunit.hrl").

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
%% TODO (AH): give a bit of structure (basic, complex, limiting...)
-export([invalid/0, valid/0]).
-export([member/1]).
-export([literal/1]).
-export([regex/1]).
-export([max_length/1]).
-export([list_of/1, map_of/2, tuple_of/1]).
-export([transform/1]).
%% Type casting validators
%% TODO: unify string and binary validators in one validator,
%% eg. atom_from_string and atom_from_binary unified into
%% to_atom(string()|binary()) -> validator(string()|binary(),
%% atom()). Use type baleen:str/0 if finally defined.

-export([integer_from_string/0]).
-export([to_atom/0]).

%% Validator "lifters"
%% TODO: to be decided/implemented
%% -spec lifter1_aka_list(list(validator(A,B))) -> validator(list(A), B).
%% -spec lifter2_aka_map(list(validator(A,B))) -> validator(list(A), list(B)).
%% -spec lifter1_aka_other_map(validator(list(A),B)) -> list(validator(A, B)).
%% -spec lifter2_aka_erlang_map(validator(map(A,B),C)) -> map(A,validator(B,C)).


%%====================================================================
%% Types
%%====================================================================

-type result(R) :: {ok, R} | {error, binary()}.

-type predicate(A) :: fun((A) -> boolean()).

-type str() :: string() | binary().

-opaque validator(A,B) :: fun((A) -> result(B)).


%%====================================================================
%% API functions
%%====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec validate(validator(A,B), A) -> result(B).
%% @doc Validates data with a validator. `X' is the term to be
%% validated with validator `V'.n
validate(V, X) -> V(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec predicate(predicate(A)) -> validator(A,A) when A :: term().
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
-spec validator(fun((A,B) -> result(A))) -> validator(A,B)
						when A :: term(), B :: term().
%% @doc Returns a validator given a user defined function that
%% validates.
validator(V) -> V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec invalid() -> validator(_,_).
invalid() -> fun(X) ->
		     {error, format("Invalid term ~p", [X])}
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
-spec compose(validator(A, B), validator(B, C)) -> validator(A, C).
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
compose(Validators) -> lists:foldr(fun compose/2, valid(), Validators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec all(list(validator(A, B))) -> validator(A, B).
all([]) -> valid();
all([V|Vs]) -> 
    fun(T) ->
	    case validate(V, T) of
		{ok, X1} ->
		    case validate(all(Vs), T) of
			{ok, X2} -> 
			    case X1 =:= X2 of
				true -> {ok, X1};
				false -> {error, format("~p and ~p are not equal", [X1, X2])}
			    end;
			{error, Error} -> {error, Error}
		    end;
		{error, Error} -> {error, Error}
	    end
    end.

all_test_() ->
    Values = ["Hello", <<"By">>, 42],
    [?_assertMatch({ok, Value},
		   validate(all([valid(), valid()]), Value))
     || Value <- Values]
	++
	[?_assertEqual({error, format("Invalid term ~p", [Value])},
		       validate(all([valid(), invalid()]), Value))
	 || Value <- Values]
	++
	[?_assertEqual({error, format("Invalid term ~p", [Value])},
		       validate(all([invalid(), valid(), invalid()]), Value))
	 || Value <- Values].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec any(list(validator(A, B))) -> validator(A, B).
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

any_test_() ->
    Values = ["Hello", <<"By">>, 42],
    [?_assertMatch({ok, Value},
		   validate(any([valid(), valid()]), Value))
     || Value <- Values]
	++
	[?_assertMatch({ok, Value},
		       validate(any([valid(), invalid()]), Value))
	 || Value <- Values]
	++
	[?_assertNotMatch({error, <<"There isn't any valid">>},
			  validate(any([invalid(), valid(), invalid()]), Value))
	 || Value <- Values]
	++
	[?_assertMatch({error, <<"There isn't any valid">>},
		       validate(any([invalid(), invalid(), invalid()]), Value))
	 || Value <- Values].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec member(list(A)) -> validator(A, A).
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
literal(Term) ->
    fun(T) ->
	    case Term =:= T of
		true -> {ok, Term};
		false -> {error, format("~p and ~p do not match", [Term, T])}
	    end
    end.

literal_test_() ->
    Values = [undefined, 1, <<"Hello">>, true, 0, -1],
    [?_assertMatch({ok, Value},
		   validate(literal(Value), Value))
     || Value <- Values]
	++
	[?_assertMatch({error, _},
		       validate(literal(false), Value))
	 || Value <- Values].

literal_1_test_() ->
    Terms = [4, undefined, <<"Bye">>, "foo"],
    Values = [3, defined, <<"Hello">>, "bar"],
    [?_assertEqual({error, format("~p and ~p do not match", [Term, Value])},
		   validate(literal(Term), Value))
     || Term <- Terms, Value <- Values].

literal_2_test_() ->
    Values = ["Hello", <<"By">>, 42],
    [if Literal =:= Value ->
	     ?_assertEqual({ok, Value}, validate(literal(Literal), Value));
	true ->
	     ?_assertMatch({error, _}, validate(literal(Literal), Value))
     end
     || Literal <- Values, Value <- Values].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec regex(String) -> validator(String, String).
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

regex_test_() ->
    Values = [<<"aab">>, <<"abababa">>],
    [?_assertMatch({ok, Value},
		   validate(regex(<<"[ab]*">>), Value))
     || Value <- Values].

regex_1_test_() ->
    Values = [<<"ababab">>, <<"aaaaaaaaaaab">>],
    REs = [<<"^c[ab]*$">>],
    [?_assertEqual({error, format("~p is not matching the regular expression ~p", [Value, RE])},
		   validate(regex(RE), Value))
     || Value <- Values, RE <- REs].

regex_2_test_() ->
    Values = [<<"ababcdcd">>, <<"ab">>],
    [?_assertMatch({ok, Value},
		   validate(regex(<<"(ab)[cd]*">>), Value))
     || Value <- Values].

regex_3_test_() ->
    Values = ["Hola"],
    REs = [<<"[ab]*">>],
    [?_assertEqual({error, format("~p is not matching the regular expression ~p", [Value, RE])},
		   validate(regex(RE), Value))
     || Value <- Values, RE <- REs].

regex_4_test_() ->
    Email_Regexp = "^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$",
    Emails = ["aherranz@gmail.com", <<"angel.herranz@coowry.com">>],
    No_Emails = ["1", "aherranz", <<"angel.herranz.coowry.com">>],
    Email_Validator = regex(Email_Regexp),
    [?_assertEqual({ok, Email}, validate(Email_Validator, Email))
     || Email <- Emails ]
	++
	[?_assertMatch({error, _}, validate(Email_Validator, No_Email))
	 || No_Email <- No_Emails ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec max_length(S) -> validator(S, S) when S :: iodata().
max_length(S) -> 
    fun(T) ->
	    case iolist_size(S) > T of
		true -> {error, format("~p is longer than ~p", [S, T])};
		false -> {ok, S}
	    end
    end.

length_test_() ->
    Values = [["He", <<"ll">>, ["o"]], "12345", "Bye"],
    [?_assertMatch({ok, Value},
		   validate(max_length(Value), 6))
     || Value <- Values].

length_1_test_() ->
    Values = [<<"123">>, "Bye", ["defined"]],
    Max_lengths = [2],
    [?_assertEqual({error, format("~p is longer than ~p", [Value, Max])},
		   validate(max_length(Value), Max))
     || Value <- Values, Max <- Max_lengths].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec to_atom() -> validator(str(), atom()).
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

to_atom_test_() ->
    Atoms = ['1234', 'Hello', 'Bye', an_atom],
    [?_assertEqual({ok, Atom},
		   validate(to_atom(), erlang:atom_to_list(Atom)))
     || Atom <- Atoms]
	++
	[?_assertEqual({ok, Atom},
		       validate(to_atom(), erlang:atom_to_binary(Atom, utf8)))
	 || Atom <- Atoms].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec list_of(validator(A,B)) -> validator(list(A), list(B)).
list_of(V) ->
    fun(L) ->
	    Results = lists:map(V, L),
	    case lists:keyfind(error, 1, Results) of
		false -> {ok, lists:map(fun(Tuple) -> element(2, Tuple) end, Results)};
		Tuple -> {error, format("There was an error in a result: ~p", [Tuple])}
	    end
    end.

list_of_test_() ->
    Values = ["1", "43", "86", "95"],
    [?_assertEqual({ok, lists:map(fun(X) -> erlang:list_to_integer(X) end, Values)},
		  validate(list_of(integer_from_string()), Values))]
	++
	[?_assertMatch({error, _},
		      validate(list_of(to_atom()), Values))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec map_of(validator(A, B), validator(A,B)) -> validator(#{A => B}, #{A => B}).
map_of(K, V) -> 
    fun(Map) ->
	    TupleList = maps:to_list(Map),
	    Results = lists:flatten(lists:map(fun(Tuple) -> 
						      erlang:tuple_to_list({K(element(1, Tuple)), V(element(2, Tuple))})
					      end, TupleList)),
	    case lists:keyfind(error, 1, Results) of
		false -> {ok, maps:from_list(compose_map(Results))};
		Tuple -> {error, format("There was an error in a result: ~p", [Tuple])}
	    end
    end.	    
	    

map_of_test_() ->
    Values = #{<<"Hello">> => "1234", <<"Bye">> => "5678"},
    [?_assertEqual({ok, #{'Bye' => 5678, 'Hello' => 1234}},
		 validate(map_of(to_atom(), integer_from_string()), Values))]
     ++
	[?_assertMatch({error, _},
		      validate(map_of(integer_from_string(), to_atom()), Values))
	].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -spec map(validator(Key,Value)) -> validator(#{Key => Value},).
% map(_M) -> invalid().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec tuple_of(validator(A, B)) -> validator({A}, {B}).
tuple_of(V) -> 
    fun(T) ->
	    Results = lists:map(V, erlang:tuple_to_list(T)),
	    case lists:keyfind(error, 1, Results) of
		false -> {ok, erlang:list_to_tuple(lists:map(fun(Tuple) -> element(2, Tuple) end, Results))};
		Tuple -> {error, format("There was an error in a result: ~p", [Tuple])}
	    end
    end.

tuple_of_test_() ->
    Values = [<<"1">>, <<"Bye">>, <<"Hello">>, <<"atom">>],
    [?_assertEqual({ok, erlang:list_to_tuple(lists:map(fun(X) -> erlang:binary_to_atom(X, utf8) end,Values))},
		  validate(tuple_of(to_atom()), erlang:list_to_tuple(Values)))]
	++
	[?_assertMatch({error, _},
		      validate(tuple_of(integer_from_string()),
					erlang:list_to_tuple(Values)))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec transform(fun((A) -> B)) -> validator(A,B).
transform(F) ->
    fun(T) ->
	    {ok, F(T)}
    end.

transform_test_() ->
    Numbers = [1, 77, 23, 50, 95],
    [?_assertEqual({ok, erlang:integer_to_list(Number)},
		   validate(transform(fun erlang:integer_to_list/1), Number))
     || Number <- Numbers].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%====================================================================
%% Internal functions
%%====================================================================
-spec format(io:format(), [term()]) -> binary().
format(Format, Terms) ->
    Message = io_lib:format(Format, Terms),
    unicode:characters_to_binary(Message).

-spec compose_map(list(tuple())) -> list(tuple()).
compose_map(L) -> lists:reverse(compose_map(L, [])).
compose_map([], Acc) -> Acc;
compose_map([H1, H2|T], Acc) -> compose_map(T, [{element(2, H1), element(2, H2)}|Acc]).
