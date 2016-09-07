-module(spawn_test).
-compile(export_all).

g(X) ->
  timer:sleep(10),
  io:format("~p~n", [X]).
