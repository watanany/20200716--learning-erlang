-module(ex08).
-compile(export_all).

start(Atom, Fun) ->
  Pid = spawn(Fun),
  register(Atom, Pid).
