-module(prop_basic_validation).
-include_lib("proper/include/proper.hrl").

-import(baleen, [validate/2, valid/0, invalid/0, to_integer/0, to_float/0]).

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

prop_is_integer_with_integer_strings() ->
  ?FORALL(Integer, integer(),
          {ok, Integer} == baleen:validate(to_integer(),
                                           integer_to_list(Integer))).

prop_is_integer_with_ingeter_binaries() ->
  ?FORALL(Integer, integer(),
          {ok, Integer} == baleen:validate(to_integer(),
                                           integer_to_binary(Integer))).

prop_is_integer_with_any_binary() ->
  ?FORALL(Binary, binary(),
          try
            Integer = binary_to_integer(Binary),
            {ok, Integer} == baleen:validate(to_integer(), Binary)
          catch
            error:badarg ->
              case baleen:validate(to_integer(), Binary) of
                {error, _} ->
                  true;
                _ -> false
              end
          end).

prop_is_float_with_float_strings() ->
  ?FORALL(Float, float(),
          {ok, Float} == baleen:validate(to_float(),
                                         float_to_list(Float))).

prop_is_float_with_ingeter_binaries() ->
  ?FORALL(Float, float(),
          {ok, Float} == baleen:validate(to_float(),
                                         float_to_binary(Float))).

prop_is_float_with_any_binary() ->
  ?FORALL(Binary, binary(),
          try
            Float = binary_to_float(Binary),
            {ok, Float} == baleen:validate(to_float(), Binary)
          catch
            error:badarg ->
              case baleen:validate(to_float(), Binary) of
                {error, _} ->
                  true;
                _ -> false
              end
          end).
