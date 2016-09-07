-module(linkmon).
-compile(export_all).

myproc() ->
  timer:sleep(5000),
  exit(reason).

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;

chain(N) ->
  Pid = spawn(fun() -> chain(N - 1) end),
  % Pid = spawn(?MODULE, chain, [N - 1]).
  link(Pid),
  receive
    _ -> ok
  end.


start_critic() ->
  spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
  Pid ! {self(), {Band, Album}},
  io:format("OUTPUT: ~p~n", [Pid]),
  receive
    {Pid, Criticism} ->
      io:format("OUTPUT: ~p~n", [Pid]),
      Criticism
  after
    2000 -> timeout
  end.

critic() ->
  receive
    {From, {"Rage Against The Turing Machine", "Unit Testify"}} ->
      From ! {self(), "They are great!"};
    {From, {_Band, _Album}} ->
      io:format("OUTPUT: ~p, ~p ~n", [From, self()]),
      From ! {self(), "They are terrible!"}
  end,
  critic().
