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

%% Types
-export_type([validator/2, result/1, predicate/1, str/0]).

%% Main validation function
-export([validate/2]).

%% Functions for user validator injections
-export([validator/1, predicate/1]).

%% Validator composition
-export([compose/2, compose/1, all/1, any/1]).

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

%% Reqpar
-export([val_map/2]).

%%====================================================================
%% Types
%%====================================================================

-type result(R) :: {ok, R} | {error, binary()}.

-type predicate(A) :: fun((A) -> boolean()).

-type str() :: string() | binary().

-opaque validator(A,B) :: fun((A) -> result(B)).

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
%% validated with validator `V'.n
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
-spec to_integer() -> validator(str(), integer()).
to_integer() ->
    fun(Value) when is_binary(Value)->
	    try erlang:binary_to_integer(Value) of
		Integer -> {ok, Integer}
	    catch
		_:_ -> {error, format("\"~w\" is not an integer", [Value])}
	    end;
       (Value) ->
	    case io_lib:fread("~d",Value) of
		{ok, [Integer], []} -> {ok, Integer};
		_ -> {error, format("\"~w\" is not an integer", [Value])}
	    end
    end.

to_integer_test_() ->
    Values = [123, 456, 3, 25, 76],
    [?_assertEqual({ok, Value},
		  validate(to_integer(), erlang:integer_to_binary(Value)))
     || Value <- Values]
	++
	[?_assertEqual({ok, Value},
		      validate(to_integer(), erlang:integer_to_list(Value)))
	|| Value <- Values].

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
-spec regex(String) -> validator(String, String) when String :: str().
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
-spec max_length(non_neg_integer()) -> validator(S, S) when S :: iodata().
max_length(I) -> 
    fun(S) ->
	    case iolist_size(S) > I of
		true -> {error, format("The size of ~p is longer than ~p", [S, I])};
		false -> {ok, S}
	    end
    end.

max_length_test_() ->
    Values = [[<<"A tip">>], "Hello", <<"Bye">>],
    [?_assertEqual({ok, Value},
		   validate(max_length(140), Value))
    || Value <- Values]
	++
	[?_assertMatch({error, _},
		      validate(max_length(0), Value))
	|| Value <- Values].

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
		  validate(list_of(to_integer()), Values))]
	++
	[?_assertMatch({error, _},
		      validate(list_of(to_atom()), Values))].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec map_of(validator(K1, K2), validator(V1, V2))
            -> validator(#{K1 => V1}, #{K2 => V2}).
map_of(VK, VV) ->
    fun(Map) ->
	    TupleList = maps:to_list(Map),
	    Results = lists:flatten(lists:map(fun({K,V}) -> 
						      erlang:tuple_to_list({VK(K), VV(V)})
					      end, TupleList)),
	    case lists:keyfind(error, 1, Results) of
		false -> {ok, maps:from_list(compose_map(Results))};
		Tuple -> {error, format("There was an error in a result: ~p", [Tuple])}
	    end
    end.	    

map_of_test_() ->
    Values = #{<<"Hello">> => "1234", <<"Bye">> => "5678"},
    [?_assertEqual({ok, #{'Bye' => 5678, 'Hello' => 1234}},
		 validate(map_of(to_atom(), to_integer()), Values))]
     ++
	[?_assertMatch({error, _},
		      validate(map_of(to_integer(), to_atom()), Values))
	].

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
		      validate(tuple_of(to_integer()),
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
-spec to_float() -> validator(str(), float()).
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
		    
to_float_test_() ->
    Values = [3.14, 17.3e-6, 0.000000000001],
    [?_assertEqual({ok, Value},
		  validate(to_float(), erlang:float_to_binary(Value)))
    || Value <- Values]
	++
	    [?_assertEqual({ok, Value},
		  validate(to_float(), erlang:float_to_list(Value)))
    || Value <- Values].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec val_map(#{K => {optional|required, validator(A,B)}},
	    #{K => A}) ->
		   #{valid => #{K => B},
		     nonvalid => #{K => binary()},
		     missing => [K],
		     unexpected => [K]}.
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

val_map_test_() ->
    Validator = #{msisdn => {required, regex("^\\+[1-9][0-9]{8,14}$")},
		  email => {optional, regex("^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$")}
		 },
    Info = #{email => "meruiz@email.com",
	     field => "Some description"},
    ?_assertEqual(#{valid => #{email => "meruiz@email.com"},
		    nonvalid => #{},
		    missing => [msisdn],
		    unexpected => [field]},
		 val_map(Validator, Info)).

val_map_1_test_() ->
    EmailRegex = "^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$",
    Validator = #{msisdn => {required, regex("^\\+[1-9][0-9]{8,14}$")},
		  email => {optional, regex(EmailRegex)},
		  concept => {optional, compose(literal("A tip for your service"), to_atom())}
		 },
    Info = #{msisdn => "+34666657231",
	     email => "meruiz.email.com",
	     concept => "A tip for your service"},
    ?_assertEqual(#{valid => #{msisdn => "+34666657231",
			       concept => 'A tip for your service'},
		    nonvalid => #{email => format("~p is not matching the regular expression ~p", ["meruiz.email.com", EmailRegex])},
		    missing => [],
		    unexpected => []},
		 val_map(Validator, Info)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%====================================================================
%% Internal functions
%%====================================================================
-spec format(io:format(), [term()]) -> binary().
format(Format, Terms) ->
    Message = io_lib:format(Format, Terms),
    unicode:characters_to_binary(Message).

-spec compose_map(list({K,V})) -> list({K,V}).
compose_map(L) -> lists:reverse(compose_map(L, [])).

-spec compose_map(list({K,V}), list({K,V})) -> list({K,V}).
compose_map([], Acc) -> Acc;
compose_map([{ok, V1}, {ok, V2}|T], Acc) -> compose_map(T, [{V1, V2}|Acc]).
