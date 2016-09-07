-module(recursion).
-export([fac/1, len/1, tail_len/1, tail_duplicate/2, tail_reverse/1, sublist/2, tail_sublist/2, tail_zip/2]).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).


tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;

tail_len([_|T], Acc) ->
  tail_len(T, Acc + 1).


tail_duplicate(N, X) when is_number(N), N >= 0 ->
  tail_duplicate(N, X, []).

tail_duplicate(0, _, Acc) -> Acc;

tail_duplicate(N, X, Acc) ->
  tail_duplicate(N - 1, X, [X|Acc]).


tail_reverse(L) when is_list(L) ->
  tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;

tail_reverse([H|T], Acc) ->
  tail_reverse(T, [H|Acc]).


sublist([], 0) -> [];
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H|T], N) -> [H|sublist(T, N - 1)].

tail_sublist(L, N) ->
  tail_sublist(L, N, []).

tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], _, Acc) -> Acc;
tail_sublist([H|T], N, Acc) ->
  tail_reverse(tail_sublist(T, N - 1, [H|Acc])).

tail_zip(L1, L2) ->
  tail_zip(L1, L2, []).

tail_zip([], _, Acc) -> Acc;
tail_zip(_, [], Acc) -> Acc;
tail_zip([], [], Acc) -> Acc;
tail_zip([H1|T1], [H2|T2], Acc) ->
  % tail_zip(T1, T2, Acc ++ [{H1, H2}]).   %% -> not effective if Acc is long list
  tail_reverse(tail_zip(T1, T2, [{H1, H2}|Acc])).
