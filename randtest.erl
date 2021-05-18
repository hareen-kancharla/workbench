%% A module for performing randomized test.

-module(randtest).

-export([generate/0, generate_type/2]).

-define(NUM_DATATYPES, 3).

generate_type(Type, X) ->
    case Type of
        1 -> generate_list(X);
        2 -> generate_tuple(X);
        3 -> generate_map(X)
        %4 -> generate_record(X-1)
    end.

generate_list(X) ->
    case X =:= 0 of
        true ->
            [rand:uniform(1024)];
        false ->
            Data = generate_type(rand:uniform(?NUM_DATATYPES), rand:uniform(X+1)-1),
            [Data, generate_list(X-1)]
    end.

generate_tuple(X) ->
    case X =:= 0 of
        true ->
            {rand:uniform(1024)};
        false ->
            Data = generate_type(rand:uniform(?NUM_DATATYPES), rand:uniform(X+1)-1),
            {Data, generate_tuple(X-1)}
    end.

generate_map(X) ->
    case X =:= 0 of
        true ->
            rand:uniform(1024);
        false ->
            Data = generate_type(rand:uniform(?NUM_DATATYPES), rand:uniform(X+1)-1),
            #{Data => generate_map(X-1)}
    end.

generate_record(X) ->
    case X =:= 0 of
        true ->
            rand:uniform(1024);
        false ->
            Data = generate_type(rand:uniform(?NUM_DATATYPES), rand:uniform(X+1)-1),
            [Data, generate_record(X-1)]
    end.

generate() ->
    M = 'io_lib',
    F = 'format',
    FileName = "./tmp.txt",
    {ok, File} = file:open(FileName, [append, raw, binary]),
    Fun = fun (A) ->
            ok = start_execution(M, F, A, File)
          end,
    % generate_recurse_term(list, Fun, N),

    Term = generate_type(1,10),
    generate_term_single(list, Fun, Term, -1),
    Term1 = generate_type(2,10),
    generate_term_single(list, Fun, Term1, -1),
    % [generate_term_single(X,10) || X = ]
    file:close(File).

generate_term_single(list, Fun, Arg, -1) ->
    generate_term_single(list, Fun, Arg, 10);
generate_term_single(list, Fun, Arg, I) ->
    case I =:= 0 of
        true -> ok;
        false ->
            Fun(["~p", [Arg], [{chars_limit, rand:uniform(1024)}]]),
            generate_term_single(list, Fun, Arg, I-1)
    end.

generate_recurse_term(list, Fun, N) ->
    case N =:= 0 of
        true ->
            [0];
        false ->
            Ns = [N, generate_recurse_term(list, Fun, N-1)],
            Fun(["~p", [Ns], [{chars_limit, 1000}]]),
            Ns
    end.

%% start a child process and run it in a timer, to see if it timesout .. if it timeout log to a file.
%% Now restart the machine with a new term ..

start_execution(M, F, A, File) ->
    Parent = self(),
    spawn(fun() ->
             erlang:apply(M, F, A),
             Parent ! eok
          end),
    receive
       eok ->
            ok
    after 2000 ->
        %% log error and continue to execute other Funs.
        file:write(File, io_lib:format("Error : ~0p~n", [A])),
        ok
    end.
