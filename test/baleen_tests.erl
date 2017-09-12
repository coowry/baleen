-module(baleen_tests).

-include_lib("eunit/include/eunit.hrl").
-import(baleen, [format/2]).
invalid_simple_test_() ->
    [ ?_assertMatch({error, <<"Invalid term",_/binary>>},
		    baleen:validate(baleen:invalid(), "Hola"))
    , ?_assertNotMatch({ok, _},
		       baleen:validate(baleen:invalid(), 42)) ].

invalid_test_() ->
    Values = [true, null, undefined, "Hola", <<"">>, <<"Hola">>, 1, 0 , -1],
    [ ?_assertMatch({error, <<"Invalid term",_/binary>>},
		    baleen:validate(baleen:invalid(), Value))
      || Value <- Values ].

valid_test_() ->
    Values = [true, null, undefined, "Hola", <<"">>, <<"Hola">>, 1, 0 , -1],
    [ ?_assertMatch({ok, Value},
		    baleen:validate(baleen:valid(), Value))
      || Value <- Values ].

to_integer_test_() ->
    Values = [123, 456, 3, 25, 76],
    [?_assertEqual({ok, Value},
		  baleen:validate(baleen:to_integer(), erlang:integer_to_binary(Value)))
     || Value <- Values]
	++
	[?_assertEqual({ok, Value},
		      baleen:validate(baleen:to_integer(), erlang:integer_to_list(Value)))
	|| Value <- Values].

any_test_() ->
    Values = ["Hello", <<"By">>, 42],
    [?_assertMatch({ok, Value},
		   baleen:validate(baleen:any([baleen:valid(), baleen:valid()]), Value))
     || Value <- Values]
	++
	[?_assertMatch({ok, Value},
		       baleen:validate(baleen:any([baleen:valid(), baleen:invalid()]), Value))
	 || Value <- Values]
	++
	[?_assertNotMatch({error, <<"There isn't any valid">>},
			  baleen:validate(baleen:any([baleen:invalid(), baleen:valid(), baleen:invalid()]), Value))
	 || Value <- Values]
	++
	[?_assertMatch({error, <<"There isn't any valid">>},
		       baleen:validate(baleen:any([baleen:invalid(), baleen:invalid(), baleen:invalid()]), Value))
	 || Value <- Values].

literal_test_() ->
    Values = [undefined, 1, <<"Hello">>, true, 0, -1],
    [?_assertMatch({ok, Value},
		   baleen:validate(baleen:literal(Value), Value))
     || Value <- Values]
	++
	[?_assertMatch({error, _},
		       baleen:validate(baleen:literal(false), Value))
	 || Value <- Values].

literal_1_test_() ->
    Terms = [4, undefined, <<"Bye">>, "foo"],
    Values = [3, defined, <<"Hello">>, "bar"],
    [?_assertMatch({error, _},
		   baleen:validate(baleen:literal(Term), Value))
     || Term <- Terms, Value <- Values].

literal_2_test_() ->
    Values = ["Hello", <<"By">>, 42],
    [if Literal =:= Value ->
	     ?_assertEqual({ok, Value}, baleen:validate(baleen:literal(Literal), Value));
	true ->
	     ?_assertMatch({error, _}, baleen:validate(baleen:literal(Literal), Value))
     end
     || Literal <- Values, Value <- Values].

regex_test_() ->
    Values = [<<"aab">>, <<"abababa">>],
    [?_assertMatch({ok, Value},
		   baleen:validate(baleen:regex(<<"[ab]*">>), Value))
     || Value <- Values].

regex_1_test_() ->
    Values = [<<"ababab">>, <<"aaaaaaaaaaab">>],
    REs = [<<"^c[ab]*$">>],
    [?_assertMatch({error, _},
		   baleen:validate(baleen:regex(RE), Value))
     || Value <- Values, RE <- REs].

regex_2_test_() ->
    Values = [<<"ababcdcd">>, <<"ab">>],
    [?_assertMatch({ok, Value},
		   baleen:validate(baleen:regex(<<"(ab)[cd]*">>), Value))
     || Value <- Values].

regex_3_test_() ->
    Values = ["Hola"],
    REs = [<<"[ab]*">>],
    [?_assertMatch({error, _},
		   baleen:validate(baleen:regex(RE), Value))
     || Value <- Values, RE <- REs].

regex_4_test_() ->
    Email_Regexp = "^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$",
    Emails = ["aherranz@gmail.com", <<"angel.herranz@coowry.com">>],
    No_Emails = ["1", "aherranz", <<"angel.herranz.coowry.com">>],
    Email_Validator = baleen:regex(Email_Regexp),
    [?_assertEqual({ok, Email}, baleen:validate(Email_Validator, Email))
     || Email <- Emails ]
	++
	[?_assertMatch({error, _}, baleen:validate(Email_Validator, No_Email))
	 || No_Email <- No_Emails ].

max_length_test_() ->
    Values = [[<<"A tip">>], "Hello", <<"Bye">>],
    [?_assertEqual({ok, Value},
		   baleen:validate(baleen:max_length(140), Value))
    || Value <- Values]
	++
	[?_assertMatch({error, _},
		      baleen:validate(baleen:max_length(0), Value))
	|| Value <- Values].

to_atom_test_() ->
    Atoms = ['1234', 'Hello', 'Bye', an_atom],
    [?_assertEqual({ok, Atom},
		   baleen:validate(baleen:to_atom(), erlang:atom_to_list(Atom)))
     || Atom <- Atoms]
	++
	[?_assertEqual({ok, Atom},
		       baleen:validate(baleen:to_atom(), erlang:atom_to_binary(Atom, utf8)))
	 || Atom <- Atoms].

list_of_test_() ->
    Values = ["1", "43", "86", "95"],
    [?_assertEqual({ok, lists:map(fun(X) -> erlang:list_to_integer(X) end, Values)},
		  baleen:validate(baleen:list_of(baleen:to_integer()), Values))]
	++
	[?_assertMatch({error, _},
		      baleen:validate(baleen:list_of(baleen:to_atom()), Values))].

map_of_test_() ->
    Values = #{<<"Hello">> => "1234", <<"Bye">> => "5678"},
    [?_assertEqual({ok, #{'Bye' => 5678, 'Hello' => 1234}},
		 baleen:validate(baleen:map_of(baleen:to_atom(), baleen:to_integer()), Values))]
     ++
	[?_assertMatch({error, _},
		      baleen:validate(baleen:map_of(baleen:to_integer(), baleen:to_atom()), Values))
	].

tuple_of_test_() ->
    Values = [<<"1">>, <<"Bye">>, <<"Hello">>, <<"atom">>],
    [?_assertEqual({ok, erlang:list_to_tuple(lists:map(fun(X) -> erlang:binary_to_atom(X, utf8) end,Values))},
		  baleen:validate(baleen:tuple_of(baleen:to_atom()), erlang:list_to_tuple(Values)))]
	++
	[?_assertMatch({error, _},
		      baleen:validate(baleen:tuple_of(baleen:to_integer()),
					erlang:list_to_tuple(Values)))].
transform_test_() ->
    Numbers = [1, 77, 23, 50, 95],
    [?_assertEqual({ok, erlang:integer_to_list(Number)},
		   baleen:validate(baleen:transform(fun erlang:integer_to_list/1), Number))
     || Number <- Numbers].
		    
to_float_test_() ->
    Values = [3.14, 17.3e-6, 0.000000000001],
    [?_assertEqual({ok, Value},
		  baleen:validate(baleen:to_float(), erlang:float_to_binary(Value)))
    || Value <- Values]
	++
	    [?_assertEqual({ok, Value},
		  baleen:validate(baleen:to_float(), erlang:float_to_list(Value)))
    || Value <- Values].

val_map_test_() ->
    Validator = #{msisdn => {required, baleen:regex("^\\+[1-9][0-9]{8,14}$")},
		  email => {optional, baleen:regex("^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$")}
		 },
    Info = #{email => "meruiz@email.com",
	     field => "Some description"},
    ?_assertEqual(#{valid => #{email => "meruiz@email.com"},
		    nonvalid => #{},
		    missing => [msisdn],
		    unexpected => [field]},
		 baleen:val_map(Validator, Info)).

val_map_1_test_() ->
    EmailRegex = "^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$",
    Validator = #{msisdn => {required, baleen:regex("^\\+[1-9][0-9]{8,14}$")},
		  email => {optional, baleen:regex(EmailRegex)},
		  concept => {optional, baleen:compose(baleen:literal("A tip for your service"), baleen:to_atom())}
		 },
    Info = #{msisdn => "+34666657231",
	     email => "meruiz.email.com",
	     concept => "A tip for your service"},
    ?_assertEqual(#{valid => #{msisdn => "+34666657231",
			       concept => 'A tip for your service'},
		    nonvalid => #{email => unicode:characters_to_binary(
				    io_lib:format("~p is not matching the regular expression ~p", ["meruiz.email.com", EmailRegex]))},
		    missing => [],
		    unexpected => []},
		 baleen:val_map(Validator, Info)).
