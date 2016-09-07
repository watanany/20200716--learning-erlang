-module(ex08_2).
-compile(export_all).

start() ->
  spawn(fun() -> loop() end).

loop() ->
  receive
    {register, Pid} ->
      loop([Pid])
  end.

loop(State) ->
  receive
    mail ->
      hd(State) ! mail
  end.
