-module(prop_complex_validation).
-include_lib("proper/include/proper.hrl").

-import(baleen, [validate/2, member/1, is_ok/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_member() ->
  ?FORALL({L, T}, {list(term()), term()},
          lists:member(T,L) == is_ok(validate(member(L),T))).
