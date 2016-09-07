-module(trade_fsm).
-behaviour(gen_fsm).

%% ユーザーが使うAPI - PUBLIC API
-export([start/1, start_link/1, trade/2, accept_trade/1,
         make_offer/2, retract_offer/2, ready/1, cancel/1]).

%% gen_fsm用のCallback関数(実装しないといけない) - CALLBACK
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4,
         % ユーザー定義の状態関数
         idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
         negotiate/3, wait/2, ready/2, ready/3]).

-record(state, {name="",
                other,
                ownitems=[],
                otheritems=[],
                monitor,
                from}).

%%% PUBLIC API
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

%% トレードを始めるように自分のFSMに依頼する。他者が受理したかを返す
trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% 他社のトレード開始依頼を受理する
accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% トレード用のアイテムとして追加する
make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% トレード依頼をキャンセルする(トレードテーブルに追加したアイテムを戻す？)
retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% トレードの準備完了だと知らせる。
%% トレード相手が準備完了にすると交換成立となる
ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% トランザクションをキャンセルする
cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%%% FSM間用のAPI
%% これらのAPIはこのファイル内でのみ使用される
%% 全ての関数はデッドロックを避けるため非同期的に実装されている

%% トレードを始めるように他者のFSMに依頼する
ask_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% 他者のFSMにアイテムがトレードテーブルに追加されたことを知らせる
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% 他者のFSMにアイテムがトレードテーブルから取り下げられたことを知らせる
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% 他者のFSMに準備完了を知らせる
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

%% 他者のFSMに準備がまだ完了していないことを知らせる
%% ('wait'状態ではないということ)
not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

%% 他者のFSMに'ready'状態への遷移を待っていることを知らせる
am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

%% 他者のFSMに'ready'状態であることを知らせる
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

%% ※ 同期的になってる
%% Commitできるかどうかを尋ねる(?)
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

%% 同期的にCommitを開始する(?)
do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

%% 他者のFSMにトレードキャンセルしたことを知らせる
notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).



%%% GEN_FSM API
init(Name) ->
  {ok, idle, #state{name=Name}}.

%% 'idle'状態はトレード完了前の状態
%% ユーザーは基本的にトレード依頼を受理し、トレード相手のPidを保存しておく
idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

%% ユーザー → FSM
%% 他者FSMに交渉を行い(受理され)、他者FSMのPidを保存しておく
idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self())<
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

%% 'idle_wait'は他者のFSMからの返信を期待し、アイテムの交渉を開始する
%% 交渉依頼をしている間、他者のFSMも交渉依頼を行う
%% お互いが同じ状態であることを想定する
idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

%% 自分のFSMがトランザクションを受理する
%% 他者のFSMにそれを知らせて、'negotiate'状態に遷移する
idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting negotiation", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.


negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiate, S#stateotheritems=add(Item, OtherItems)};
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "Other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

