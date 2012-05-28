cube:luerl hd$ make minibench
----------------------------------
Compiling and running minibench.erl:
cd ./examples/minibench/ \
&& erlc minibench.erl \
&& erl -pa ../../ebin -s minibench run -s init stop -noshell
minibench.erl:102: Warning: variable 'Chunk' is unused
----------------------------------------------------------
This is a benchmark of frequent fast calls into Luerl.
----------------------------------------------------------
Init state, parse and execute '1 + 1'
Adding Up: 293226 microseconds for 1000 x calling Lua and returning the result of 1 + 1.
Per call: 293.226 microseconds.
----------------------------------------------------------
Init state, and execute pre-parsed '1 + 1'
Adding Up: 252817 microseconds for 1000 x calling Lua and returning the result of 1 + 1.
Per call: 252.817 microseconds.
----------------------------------------------------------
Execute pre-parse execute '1 + 1', re-using same state
Adding Up: 13039 microseconds for 1000 x calling Lua and returning the result of 1 + 1.
Per call: 13.039 microseconds.
----------------------------------------------------------
Pure initialization of Lua state
Adding Up: 214536 microseconds for 1000 x initializing Lua state.
Per call: 214.536 microseconds.
----------------------------------------------------------
Execute pre-parsed '1 + 1', re-using state from last result
Adding Up: 12879 microseconds for 1000 x calling Lua and returning the result of 1 + 1.
Per call: 12.879 microseconds.
----------------------------------------------------------
Pure parsing
Adding Up: 13899 microseconds for 1000 x calling Lua and returning the result of 1 + 1.
Per call: 13.899 microseconds.
----------------------------------------------------------
Parse and execute '1 + 1', re-using state
Adding Up: 29970 microseconds for 1000 x calling Lua and returning the result of 1 + 1.
Per call: 29.97 microseconds.
cube:luerl hd$ make minibench2
----------------------------------
Compiling and running minibench2.erl:
cd ./examples/minibench \
&& erlc minibench2.erl \
&& erl -pa ../../ebin -s minibench2 run -s init stop -noshell
minibench2.erl:84: Warning: variable 'T5b' is unused
minibench2.erl:122: Warning: variable 'Chunk' is unused
----------------------------------------------------------
This is a benchmark of frequent fast calls into Luerl.
----------------------------------------------------------
Init state, parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'
Adding Up: 388433 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 388.433 microseconds.
----------------------------------------------------------
Init state, and execute pre-parsed 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'
Adding Up: 245311 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 245.311 microseconds.
----------------------------------------------------------
Execute pre-parse execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using same state
Adding Up: 38124 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 38.124 microseconds.
----------------------------------------------------------
Pure initialization of Lua state
Adding Up: 195263 microseconds for 1000 x initializing a Lua state.
Per call: 195.263 microseconds.
----------------------------------------------------------
Execute pre-parsed 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result
Adding Up: 34159 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 34.159 microseconds.
----------------------------------------------------------
Execute pre-parsed function with 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result
-----------------------------------------------------------
Execute empty function, re-using state from last result
-Adding Up: 27953 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 27.953 microseconds.
----------------------------------------------------------
Pure parsing
Adding Up: 62965 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 62.965 microseconds.
----------------------------------------------------------
Parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state
Adding Up: 95812 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 95.812 microseconds.
cube:luerl hd$ make minibench2
----------------------------------
Compiling and running minibench2.erl:
cd ./examples/minibench \
&& erlc minibench2.erl \
&& erl -pa ../../ebin -s minibench2 run -s init stop -noshell
minibench2.erl:125: Warning: variable 'Chunk' is unused
----------------------------------------------------------
This is a benchmark of frequent fast calls into Luerl.
----------------------------------------------------------
Init state, parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'
Adding Up: 366619 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 366.619 microseconds.
----------------------------------------------------------
Init state, and execute pre-parsed 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b'
Adding Up: 249480 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 249.48 microseconds.
----------------------------------------------------------
Execute pre-parse execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using same state
Adding Up: 37377 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 37.377 microseconds.
----------------------------------------------------------
Pure initialization of Lua state
Adding Up: 192184 microseconds for 1000 x initializing a Lua state.
Per call: 192.184 microseconds.
----------------------------------------------------------
Execute pre-parsed 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result
Adding Up: 34630 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 34.63 microseconds.
----------------------------------------------------------
Execute pre-parsed function with 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state from last result
-Adding Up: 62865 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 62.865 microseconds.
----------------------------------------------------------
Execute empty function, re-using state from last result
-Adding Up: 31183 microseconds for 1000 x calling empty function.
Per call: 31.183 microseconds.
----------------------------------------------------------
Pure parsing
Adding Up: 59094 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 59.094 microseconds.
----------------------------------------------------------
Parse and execute 'a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b', re-using state
Adding Up: 96667 microseconds for 1000 x calling Lua and returning the result of a = 7.33; b = 9000; c = (33 * a / b) ^ 15 * a + b.
Per call: 96.667 microseconds.
cube:luerl hd$ 

