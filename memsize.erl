%% The module is not be used seperately - it was just
%% my own hacky set of functions to generate and use
%% large terms to deduce performance of different memory size
%% function in erlang like erts_debug:flat_size, erlang:byte_size.

%% Clearly there are dependencies from the ns_server app that are
%% needed for this to compile.
%%
%% The CSV formatter was picked from here - https://github.com/manastech/erl_csv_generator

-module(memsize).

-export([master_node/0,
         recurse_large_term/2,
         print_large_term/1,
         print_uniform_data_list/1,
         print_uniform_data_binary/1,
         print_random_data_list/1,
         print_random_data_binary/1, hkhk/1]).

print_large_term(X) ->
    ?log_error("HKHK Complete: formatting testing - ~p", [X]).

recurse_large_term(X, Acc) ->
    case X of
        0 ->
            Acc;
        _ ->
            recurse_large_term(X-1, [{{X, "large_term"}, Acc} | {Acc}])
    end.

%%% CSV formatting %%%

newline(File) ->
  file:write(File, "\n").

comma(File) ->
  file:write(File, ",").

field(File, Value) when is_tuple(Value) ->
  file:write(File, "\""),
  file:write(File, io_lib:format("~p",[Value])),
  file:write(File, "\"");
field(File, Value) when is_binary(Value) ->
  Match = binary:match(Value, [<<",">>, <<"\n">>, <<"\"">>]),
  case Match of
    nomatch ->
      file:write(File, Value);
    _ ->
      file:write(File, "\""),
      file:write(File, binary:replace(Value, <<"\"">>, <<"\"\"">>, [global])),
      file:write(File, "\"")
  end;
field(File, Value) when is_list(Value) ->
 field(File, unicode:characters_to_binary(Value));
field(File, Value) when is_integer(Value) ->
  file:write(File, integer_to_list(Value));
field(File, Value) when is_atom(Value) ->
  file:write(File, io_lib:write_atom(Value));
field(File, Value) when is_float(Value) ->
  file:write(File, io_lib:format("~f",[Value]));
field(File, Value) ->
  file:write(File, io_lib:format("\"~p\"",[Value])).

row(File, Elem) when is_tuple(Elem) ->
  ListElem = tuple_to_list(Elem),
  row(File, ListElem);
row(File, []) ->
  newline(File);
row(File, [Value]) ->
  field(File, Value),
  newline(File);
row(File, [Value | Rest]) ->
  field(File, Value),
  comma(File),
  row(File, Rest).

%%% CSV formatting %%%

%% Data Unit generating functions.

uniform_kilobytes(N, Val) ->
    kilo_byte(N, [Val]).

kilo_byte(0, Acc) ->
    Acc;
kilo_byte(N, Acc) ->
    kilo_byte(N-1, [Acc|Acc]).

random_kilobytes(Size) ->
    [rand:uniform(Size) || _ <- lists:seq(1,Size)].

print_random_data_list(Size) ->
    Data = ?cut(random_kilobytes(Size)),
    print(Data, Size, "random_data_list_erts_flat_size.txt", ?cut(_), {erts_debug, flat_size}),
    print(Size, Size, "random_data_list_erts_size.txt", ?cut(_), {erts_debug, size}),
    print(Size, Size, "random_data_list_byte_size.txt", ?cut(_), {erlang, byte_size}).

print_random_data_binary(Size) ->
    Data = ?cut(random_kilobytes(Size)),
    print(Data, Size, "random_data_binary_erts_flat_size.txt", ?cut(list_to_binary(_)), {erts_debug, flat_size}),
    print(Data, Size, "random_data_binary_erts_size.txt", ?cut(list_to_binary(_)), {erts_debug, size}),
    print(Data, Size, "random_data_binary_byte_size.txt", ?cut(list_to_binary(_)), {erlang, byte_size}).

print_uniform_data_list(Size) ->
    Data = ?cut(uniform_kilobytes(10, [0])),
    print(Data, Size, "uniform_data_list_erts_flat_size.txt", ?cut(_), {erts_debug, flat_size}),
    print(Size, Size, "uniform_data_list_erts_size.txt", ?cut(_), {erts_debug, size}),
    print(Size, Size, "uniform_data_list_byte_size.txt", ?cut(_), {erlang, byte_size}).

print_uniform_data_binary(Size) ->
    Data = ?cut(uniform_kilobytes(10, [0])),
    print(Data, Size, "uniform_data_binary_erts_flat_size.txt", ?cut(list_to_binary(_)), {erts_debug, flat_size}),
    print(Data, Size, "uniform_data_binary_erts_size.txt", ?cut(list_to_binary(_)), {erts_debug, size}),
    print(Data, Size, "uniform_data_binary_byte_size.txt", ?cut(list_to_binary(_)), {erlang, byte_size}).

custom_print(File, DataFun, {M,F}) ->
    fun (Data, KBSize) ->
        {Time, Result} = timer:tc(M, F, [DataFun(Data)]),
        row(File, {KBSize, Time, Result})
    end.


print(Data, Size, FileName, DataFun, SizeFun) ->
    KB = Data(),
    case file:open(filename:join("/Users/hareen.kancharla/cb/src-master-2/ns_server/data/n_0", FileName), [raw,binary,write]) of
        {ok, File} ->
            try
                FileFun = custom_print(File, DataFun, SizeFun),
                print_data(1, Size, Data, [KB], FileFun)
            after
                catch(file:close(File))
            end;
        Error ->
            Error
    end.

print_data(X, Size, Data, Rest, FileFun) ->
    case X < Size of 
        false ->
            FileFun(Rest, X);
        true ->
            FileFun(Rest, X),
            print_data(X+1, Size, Data, Data() ++ Rest, FileFun)
    end.

-spec hkhk(term()) -> allowed | not_allowed.
hkhk(Input) ->
    case Input of
        0 ->
            list_to_atom(Input);
        _ ->
            allowed
    end.

