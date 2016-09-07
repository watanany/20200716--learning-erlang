-module(functions).
-compile(export_all).

%% this functions(right_age, right_age2) are different

right_age(X) when hd(X) >= 16; X =< 104 ->
  true;
right_age(_) ->
  false.

right_age_2(X) when hd(X) >= 16 orelse X =< 104 ->
  true;
right_age_2(_) ->
  false.
