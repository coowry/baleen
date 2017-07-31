-module(prop_validator_composition).
-include_lib("proper/include/proper.hrl").

-import(baleen, [validate/2, predicate/1, compose/1, to_integer/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_compose_integer_even() ->
  ?FORALL(Integer, integer(),
          case validate(
                 compose([to_integer(),
                          predicate(fun is_even/1)]),
                 integer_to_list(Integer)) of
            {ok, Integer} ->
              0 == Integer rem 2;
            {error, _} ->
              0 /= Integer rem 2
          end).

is_even(X) when is_integer(X) -> X rem 2 == 0.
