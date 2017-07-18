-module(prop_basic_validation).
-include_lib("proper/include/proper.hrl").

-import(baleen, [validate/2, valid/0, invalid/0, integer_from_string/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_valid() ->
  ?FORALL(Data, term(),
          begin
            {ok, Data} == baleen:validate(valid(), Data)
          end).

prop_invalid() ->
  ?FORALL(Data, term(),
          case baleen:validate(invalid(), Data) of
            {error, _} -> true;
            {ok,_} -> false
          end).

prop_is_integer_with_integers() ->
  ?FORALL(Integer, integer(),
          {ok, Integer} == baleen:validate(integer_from_string(),
                                           integer_to_list(Integer))).
