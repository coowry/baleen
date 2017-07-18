-module(prop_complex_validation).
-include_lib("proper/include/proper.hrl").

-import(baleen, [validate/2, member/1, is_ok/1, is_error/1, invalid/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_member() ->
  ?FORALL({L, T}, {list(term()), term()},
          lists:member(T,L) == is_ok(validate(member(L),T))).

prop_empty_member() ->
  ?FORALL(T, term(),
          case validate(member([]),T) of
            {ok, T} -> {ok, T} == validate(invalid(),T);
            {error, _} -> is_error(validate(invalid(),T))
          end).


