-module(prop_validator_composition).
-include_lib("proper/include/proper.hrl").

-import(baleen, [validate/2, predicate/1, chain/1, integer_from_string/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_chain_integer_even() ->
  ?FORALL(Integer, integer(),
          case validate(
                 chain([integer_from_string(),
                        predicate(fun is_even/1)]),
                 integer_to_list(Integer)) of
            {ok, Integer} ->
              0 == Integer rem 2;
            {error, _} ->
              1 == Integer rem 2
          end).

is_even(X) when is_integer(X) -> X rem 2 == 0.
