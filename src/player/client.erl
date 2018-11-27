%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十一月 2018 下午3:42
%%%-------------------------------------------------------------------
-module(client).
-author("dhcd").

%% include client define
-include("../include/game_client.hrl").
-include("../../include/game_client_pb.hrl").

-behaviour(gen_server).

%% client api
-export([connect/0,
  request/2, console_detail/0]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock :: gen_tcp:socket()}).

%%%===================================================================
%%% API
%%%===================================================================

%% 客户连接请求，每次连接请求，在game_client_sup下创建新的客户端连接，便于管理连接
%% 通过tcp连接指定ip：port获得socket连接凭证，以便后续消息请求
-spec connect() -> {ok, Pid :: pid()}.
connect() ->
  supervisor:start_child(game_client_sup, []).

%% 客户端请求，向服务器发送消息请求
-spec request(Client :: pid(), Req :: term()) -> any().
request(Client, Req) ->
  lager:info("[~p]/[~p] request server the data is [~p]~n", [Client, self(), Req]),
  gen_server:call(Client, {request, Req}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  %% 创建连接 保存连接凭证
  case gen_tcp:connect(?HOST, ?PORT, ?SOCKET_OPTS) of
    {ok, Sock} ->
      lager:info("[~p] tcp request [~p:~p] success, response socket is ~p~n", [self(), ?HOST, ?PORT, Sock]),
      {ok, #state{sock = Sock}};
    {error, Reason} ->
      lager:error("[~p] tcp request [~p:~p] failure by ~p~n", [self(), ?HOST, ?PORT, Reason]),
      {stop, Reason}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call({request, Req}, _From, State) ->
  case State#state.sock of
    undefine ->
      lager:error("[~p] can't request [~p] to [~p:~p] by no socket ~n", [self(), Req, ?HOST, ?PORT]),
      {reply, unconnected, State};
    Socket ->
      lager:info("[~p] request [~p] to [~p:~p] by [~p]~n", [self(), Req, ?HOST, ?PORT, Socket]),
      gen_tcp:send(Socket, term_to_binary(Req)),
      {reply, ok, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info({tcp, _Sock, Rep}, State) ->
  case rep:handle_rep(binary_to_term(Rep)) of
    {200, Data} ->
      lager:info("[~p] request success, receive ~p~n", [self(), Data]),
      case Data of
         login_out ->
           {stop, login_out};
         {message, Msg} ->
           lager:info("[~p] receive message from [~p] : [~p]", [self(), maps:get(from, Msg), maps:get(msg, Msg)]),
           {noreply, State};
         _ ->
           {noreply, State}
      end;
    {500, _} ->
      lager:error("[~p]request fail, try again later ~n", [self()]),
      {noreply, State};
    {400, Data} ->
      case Data of
        error_param ->
          lager:warning("[~p]operation fail, try again later ~n", [self()]);
        had_registed ->
          lager:warning("[~p]account had registed, try another name ~n", [self()]);
        un_register ->
          lager:warning("[~p]account does not exist, try another name or check login info~n", [self()]),
          lager:warning("[~p]default role [~p]~n", [self(), ["humanity", "mage", "elf", "orc", "angel"]]);
        login_or_register ->
          lager:warning("[~p]please login or register~n", [self()]);
        _ ->
          lager:error("[~p]unknow operation~n", [self()])
      end,
      {noreply, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
console_detail() ->
  lager:info("%%%========================================================================================================================================================================================================="),
  lager:info("%%% send request by client:request(Pid, Req), Pid return by client:connect(),Req details are as follows"),
  lager:info("%%% 1-login "),
  lager:info("%%% Req format ~p~n", [#{oper => login, data => #{name => "****", pass => "*****", role => "*****"}}]),
  lager:info("%%% 2-register "),
  lager:info("%%% Req format ~p~n", [#{oper => register, data => #{name => "****", pass => "*****", role => "*****"}}]),
  lager:info("%%% 3-login out "),
  lager:info("%%% Req format ~p~n", [#{oper => login_out, data => #{name => "****", pass => "*****", role => "*****"}}]),
  lager:info("%%% 4-player upgrade "),
  lager:info("%%% Req format ~p~n", [#{oper => upgrade, data => #{}}]),
  lager:info("%%% 5-change name "),
  lager:info("%%% Req format ~p~n", [#{oper => change_name, data => #{new_name => "****"}}]),
  lager:info("%%% 6-change password "),
  lager:info("%%% Req format ~p~n", [#{oper => change_pass, data => #{new_pass => "****"}}]),
  lager:info("%%% 7-chang role "),
  lager:info("%%% Req format ~p~n", [#{oper => change_role, data => #{new_role => "****"}}]),
  lager:info("%%% 8-add tools "),
  lager:info("%%% Req format ~p~n", [#{oper => add_tools, data => #{tools => [#{name => "***", lv => 0, logo => 0}]}}]),
  lager:info("%%% 9-delete tool "),
  lager:info("%%% Req format ~p~n", [#{oper => delete_tools, data => #{tool => #{name => "***", lv => 0, logo => 0}}}]),
  lager:info("%%% 10-upgrade tool "),
  lager:info("%%% Req format ~p~n", [#{oper => upgrade_tools, data => #{tool => #{name => "***", lv => 0, logo => 0}}}]),
  lager:info("%%% 11-add equips "),
  lager:info("%%% Req format ~p~n", [#{oper => add_equips, data => #{equips => [#{name => "***", lv => 0, logo => 0}]}}]),
  lager:info("%%% 12-delete equip "),
  lager:info("%%% Req format ~p~n", [#{oper => delete_equips, data => #{equip => #{name => "***", lv => 0, logo => 0}}}]),
  lager:info("%%% 13-upgrade equip "),
  lager:info("%%% Req format ~p~n", [#{oper => upgrade_equips, data => #{equip => #{name => "***", lv => 0, logo => 0}}}]),
  lager:info("%%% 14-player info equip "),
  lager:info("%%% Req format ~p~n", [#{oper => info, data => #{}}]),
  lager:info("%%% 15-send msg equip "),
  lager:info("%%% Req format ~p~n", [#{oper => send_msg, data => #{from => "***", target => "***", msg => "***"}}]),
  lager:info("%%%=========================================================================================================================================================================================================").