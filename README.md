# Baleen

Data validation in Erlang

## Introduction

Baleen is a `rebar3` library in order to validate the content of the data that is being inputted
to your programs.

## Validators

In order to validate the data, Baleen uses validators, functions that ensures if the content is correct or not.
Our definition of `validator`is a function that takes a data of `A` type and returns a `result(B)`,
this is, a tuple like `{ok, B}` or `{error, binary()}`. We'll explain later them in the Types section.

## Types

We define 3 types:

* result(R):
	
	It's a tuple with the form `{ok, R}` or `{error, binary()}`, where the `binary()` is a message with a brief explanation of
	why the data is invalid.
		
* predicate(A):

	It's a function that takes an `A` type and returns a `boolean()`.

* str():

	It's a type that can be a `binary()` or a `string()`.

## Functions

### any(validator(A,B)) -> validator(list(A), B)

Returns a validator of a list of validators, the first that matches.

Examples:
```erlang
1> ValidatorList = baleen:any([baleen:invalid(), baleen:invalid(), baleen:valid()]).
2> baleen:validate(ValidatorList, 42).
{ok, 42}
```
```erlang
1> Validator = baleen:any([baleen:max_length(0), baleen:to_atom(), baleen:to_float()]).
2> baleen:validate(Validator, "Error").
{error,<<"There isn't any valid">>}
```

### compose(nonempty_list(validator(A,A))) -> validator(A,A)

Returns a validator that is a composition of a list of validators.

Examples:
```erlang
1> Validator = baleen:compose([baleen:max_length(10), baleen:to_integer(),baleen:validator(fun(I) -> case is_integer(I) of true -> {ok, I}; false -> {error, <<"Is not an integer">>} end end)]).
2> baleen:validate(Validator, "12345").
{ok, 12345}
3> baleen:validate(Validator, 12345678900).
{error,<<"The size of \"12345678900\" is longer than 10">>}
```


### compose(validator(A, B), validator(B, C)) -> validator(A, C)

Returns a validator that is a composition of two validators.

Examples:
```erlang
1> Validator = baleen:compose(baleen:max_length(140), baleen:to_atom()).
2> baleen:validate(Validator, <<"Hello Mike">>).
{ok,'Hello Mike'}
```
### invalid() -> validator(_,_)

Returns a validator that always fails.

Examples:
```erlang
1> Validator = baleen:invalid().
2> baleen:validate(Validator, undefined).
{error,<<"Invalid term undefinded">>}
```

### list_of(validator(A,B)) -> validator(list(A), list(B))

Returns a validator that matches a list.

Examples:
```erlang
1> Validator = baleen:list_of(baleen:to_integer()).
2> baleen:validate(Validator, ["1", "2", "3"]).
{ok,[1,2,3]}
3> baleen:validate(Validator, ["4", "Ooops", "6"]).
{error,<<"Error in element \"Ooops\": \"Ooops\" is not an integer">>}
```


### literal(A) -> validator(A, A)

Returns a validator that matches only if the input is equals to `Term`.

Examples:
```erlang
1> Validator = baleen:literal(42).
2> baleen:validate(Validator, 42).
{ok, 42}
```
```erlang
1> Validator = baleen:literal("String").
2> baleen:validate(Validator, "string").
{error,<<"\"String\" and \"string\" do not match">>}
```


### map_of(validator(K1, K2), validator(V1, V2)) -> validator(#{K1 => V1}, #{K2 => V2})

Returns a validator that validates a map, whose Keys are validated by `validator(K1, K2)`
and Values are validated by `validator(V1, V2)`. 

Examples:
```erlang
1> KeyValidator = baleen:to_atom().
2> ValueValidator = baleen:to_integer().
3> Validator = baleen:map_of(KeyValidator, ValueValidator).
4> baleen:validate(Validator, #{<<"one">> => "1", <<"two">> => "2", <<"three">> => "3"}).
{ok,#{one => 1,three => 3,two => 2}}
5> baleen:validate(Validator, #{one => "1", <<"two">> => "2", <<"three">> => "3"}).
{error,<<"Error in key one: one is not a valid string">>}
6> baleen:validate(Validator, #{"one" => "1", <<"two">> => "Two", <<"three">> => "3"}).
{error,<<"Error in value \"Two\": \"Two\" is not an integer">>}
```


### max_length(non_neg_integer()) -> validator(S, S) when S :: iodata().

Returns a validator that validates an input only if its length is less or equal than the integer is specified.

Examples:
```erlang
1> Validator = baleen:max_length(50).
2> baleen:validate(Validator, "A short message").
{ok,"A short message"}
```
```erlang
1> Validator = baleen:max_length(10).
2> baleen:validate(Validator, "A long message").
{error,<<"The size of \"A long message\" is longer than 10">>}
```

### member(list(A)) -> validator(A, A)

Returns a validator that matches only if the input is member of `L`.

Examples:
```erlang
1> Validator = baleen:member(["Hello", 1, 0, -1, "Bye", atom]).
2> baleen:validate(Validator, atom).
{ok, atom}
3> baleen:validate(Validator, 2).
{error,<<"2 is not member of [\"Hello\",1,0,-1,\"Bye\",atom]">>}
```


### predicate(predicate(A)) -> validator(A,A)

Returns a validator given a predicate of type `predicate`.

Examples:
```erlang
1> Pred = fun(X) -> case is_atom(X) of true -> true; false -> false end end. 
2> baleen:validate(baleen:predicate(Pred), atom).
{ok, atom}
3> baleen:validate(baleen:predicate(Pred), 1).
{error,<<"Improper term 1">>}
```


### regex(String) -> validator(String, String) when String :: str()

Returns a validator given a regular expression.

Examples:
```erlang
1> Validator = baleen:regex("^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$").
2> baleen:validate(Validator, "user@email.com").
{ok, "user@email.com"}
3> baleen:validate(Validator, "user.email.com").
{error,<<"\"user.email.com\" is not matching the regular expression \"^([0-9a-zA-Z]([-\\\\.\\\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z]["...>>}
```

### to_atom() -> validator(str(), atom())

Returns a validator in order to cast to atom.

Examples:
```erlang
1> Validator = baleen:to_atom().
2> baleen:validate(Validator, <<"atom">>).
{ok, atom}
3> Variable = 'String'. %% The atom must exists.
4> baleen:validate(Validator, "String").
{ok, 'String'}
5> baleen:validate(Validator, "Not a valid string").
{error,<<"\"Not a valid string\" is not a valid string">>}
```

### to_float(str(), float())

Returns a validator in order to cast to float.

Examples:
```erlang
1> Validator = baleen:to_float().
2> baleen:validate(Validator, <<"123.75646">>).
{ok,123.75646}
3> baleen:validate(Validator, "3.14159265359").
{ok,3.14159265359}
4> baleen:validate(Validator, "Hello").
{error,<<"\"\"Hello\"\" is not a float">>}
```


### to_integer() -> validator(str(), integer())

Returns a validator in order to cast to integer.

Examples:
```erlang
1> Validator = baleen:to_integer().
2> baleen:validate(Validator, "42").
{ok, 42}
3> baleen:validate(Validator, <<"7">>).
{ok, 7}
4> baleen:validate(Validator, <<"Hello">>).
{error,<<"<<\"Hello\">> is not an integer">>}
```


### transform(fun((A) -> B)) -> validator(A,B)

Returns a validator that always success and applies `F`.

Examples:
```erlang
1> Validator = baleen:transform(fun erlang:integer_to_list/1).
2> baleen:validate(Validator, 23).
{ok, "23"}
```

### tuple_of(validator(A, B)) -> validator({A}, {B})

Returns a validator that matches a tuple.

Examples:
```erlang
1> Validator = baleen:tuple_of(baleen:to_atom()).
2> baleen:validate(Validator, {<<"hello">>, <<"1234">>, <<"bye">>}).
{ok,{hello,'1234',bye}}
3> baleen:validate(Validator, {atom, <<"1234">>, <<"bye">>}).
{error,<<"Error in atom: atom is not a valid string">>}
```


### val_map(#{K => {optional|required, validator(A,B)}}, #{K => A}) -> #{valid => #{K => B}, nonvalid => #{K => binary()}, missing => [K], unexpected => [K]}.


Returns a map with the results of the validation of the maps passed as arguments.

Examples:
```erlang
1> ValidatorMap = #{msisdn => {required, baleen:regex("^\\+[1-9][0-9]{8,14}$")},
		  email => {optional, baleen:regex("^([0-9a-zA-Z]([-\\.\\w]*[0-9a-zA-Z])*@([0-9a-zA-Z][-\\w]*[0-9a-zA-Z]\\.)+[a-zA-Z]{2,9})$")}
		 }.
2> Map = #{email => "user@email.com", field => "Some description"}.
3> baleen:val_map(ValidatorMap, Map).
#{missing => [msisdn],
  nonvalid => #{},
  unexpected => [field],
  valid => #{email => "user@email.com"}}
```


### valid() -> validator(_,_)

Returns a validator that always success.

Examples:
```erlang
1> Validator = baleen:valid().
2> baleen:validate(Validator, 1234).
{ok, 1234}
```


### validate(validator(A,B), A) -> result(B)

Validates data with a validator.

Examples:
```erlang
1> baleen:validate(baleen:valid(), 42).
{ok, 42}
```

### validator(fun((A) -> result(B))) -> validator(A,B)

Returns a validator given a user defined function that validates.

Examples:
```erlang
1> Fun = fun(I) -> case is_integer(I) of true -> {ok, I}; false -> {error, <<"Is not an integer">>} end end.
2> Validator = baleen:validator(Fun).
3> baleen:validate(Validator, 23).
{ok, 23}
4> baleen:validate(Validator, "23").
{error, <<"Is not an integer">>}
```

## Installation and usage

Baleen is a `rebar3` project:

If you want to use it and test it on your own, just clone the repository and type

```console
rebar3 shell
```

If you want to use it in your code, just add the dependency in your `rebar.config` file:

```erlang
{deps, [
	{baleen, {git, "https://github.com/coowry/baleen.git"}}
]}.
```


## License

Copyright(c) 2017, Ángel Herranz, Miguel E. Ruiz.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. All advertising materials mentioning features or use of this software
   must display the following acknowledgement:
   This product includes software developed by Coowry Ltd .

4. Neither the name of the copyright holders nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Inspiration

Baleen is inspirated in [Saul](https://github.com/whatyouhide/saul), a data validation library for Elixir.

## The name

    Baleen is a filter-feeder system inside the mouths of baleen
    whales. The baleen system works by a whale opening its mouth
    underwater and taking in water.

    https://en.wikipedia.org/wiki/Baleen
