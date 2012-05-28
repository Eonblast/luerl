%% File    : minibench2.erl
%% Author  : Henning Diedrich
%% File    : luerl/examples/minibench/minibench.erl
%% Purpose : Benchmark for frequent calls to small Luerl scripts
%% Author  : Henning Diedrich
%% Use     $ cd ./examples/minibench 
%%         $ erlc minibench.erl 
%%         $ erl -pa ../../ebin -s minibench run -s init stop -noshell
%% Or      $ make minibench

-module(minibench2).
-export([run/0]).

run() ->

    io:format("----------------------------------------------------------~n"),
    io:format("This is a benchmark of frequent fast calls into Luerl.~n"),

    % I. eval and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'~n"),
    I1 = 1000,
    {T1,_State} = timer:tc(fun() -> [luerl:do("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c") || _ <- lists:seq(1,I1)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T1,I1]),
    io:format("Per call: ~p microseconds.~n", [T1/I1]),
    

    % II. eval once, then only execute
    io:format("----------------------------------------------------------~n"),
    io:format("Init state, and execute pre-parsed 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'~n"),
    I2 = 1000,
    {ok, Chunk2} = luerl:load("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c"),
    
    {T2,_State21} = timer:tc(fun() -> [luerl:do(Chunk2) || _ <-     lists:seq(1,I2)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T2,I2]),
    io:format("Per call: ~p microseconds.~n", [T2/I2]),
    

    % III. eval once, then only execute
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parse execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using same state~n"),
    I3 = 1000,
    {ok, Chunk3} = luerl:load("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c"),
    State3 = luerl:init(),
    
    {T3,_State31} = timer:tc(fun() -> [luerl:do(Chunk3, State3) || _ <- lists:seq(1,I3)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T3,I3]),
    io:format("Per call: ~p microseconds.~n", [T3/I3]),


    % IV. measure but state initialization
    io:format("----------------------------------------------------------~n"),
    io:format("Pure initialization of Lua state~n"),
    I4 = 1000,
    
    {T4,_State41} = timer:tc(fun() -> [luerl:init() || _ <- lists:seq(1,I4)] end),

    io:format("Adding Up: ~p microseconds for ~p x initializing a Lua state.~n", [T4,I4]),
    io:format("Per call: ~p microseconds.~n", [T4/I4]),


    % V. eval once, then only execute, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result~n"),
    I5 = 1000,
    {ok, Chunk5} = luerl:load("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c"),
    State5 = luerl:init(),
    
    {T5,_State51} = timer:tc(fun() -> selffeed(State5, Chunk5, I5) end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T5,I5]),
    io:format("Per call: ~p microseconds.~n", [T5/I5]),

    % Vb. function call, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("Execute pre-parsed function with 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result~n"),
    I5b = 1000,
    State5b = luerl:init(),
    {[],State5b1} = luerl:do("function OneAndOne() a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c end", State5b),
    io:format("-"),
    {T5b,_State5b1} = timer:tc(fun() -> [{[9000.0], _} = luerl:do("return OneAndOne()", State5b1) || _ <- lists:seq(1,I5b)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T5b,I5b]),
    io:format("Per call: ~p microseconds.~n", [T5b/I5b]),

    % Vc. empty function call, re-use previous state
    io:format("----------------------------------------------------------~n"),
    io:format("Execute empty function, re-using state from last result~n"),
    I5c = 1000,
    State5c = luerl:init(),
    {[],State5c1} = luerl:do("function EmptyFunc() end", State5c),
    io:format("-"),
    {T5c,_State5c1} = timer:tc(fun() -> [{[], _} = luerl:do("EmptyFunc()", State5c1) || _ <- lists:seq(1,I5c)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling empty function.~n", [T5c,I5c]),
    io:format("Per call: ~p microseconds.~n", [T5c/I5c]),
    
    % VI. measure but parsing
    io:format("----------------------------------------------------------~n"),
    io:format("Pure parsing~n"),
    I6 = 1000,
    {T6,_State61} = timer:tc(fun() -> [luerl:load("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c") || _ <- lists:seq(1,I6)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T6,I6]),
    io:format("Per call: ~p microseconds.~n", [T6/I6]),


    % VII. Parse and execute
    io:format("----------------------------------------------------------~n"),
    io:format("Parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state~n"),
    I7 = 1000,
    State7 = luerl:init(),
    {T7,_State71} = timer:tc(fun() -> [luerl:do("a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b; return c", State7) || _ <- lists:seq(1,I7)] end),

    io:format("Adding Up: ~p microseconds for ~p x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.~n", [T7,I7]),
    io:format("Per call: ~p microseconds.~n", [T7/I7]),


    done.

% helper
selffeed(State, Chunk, 0) -> State;
selffeed(State, Chunk, I) -> 
    {_,State1} = luerl:do(Chunk, State),
    selffeed(State1, Chunk, I-1).