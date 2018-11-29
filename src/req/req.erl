%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十一月 2018 下午6:28
%%%-------------------------------------------------------------------
-module(req).
-author("dhcd").

%% API
-export([handle_req/1, login/3, register/3, login_out/3, upgrade/0, change_name/1, change_pass/1, change_role/1, te/3, add_tools/1, delete_tools/1, upgrade_tools/1, add_equips/1, delete_equips/1, upgrade_equips/1, info/0, send_msg/3]).

-include("../../include/game_client_pb.hrl").

-export_type([req/0]).

-type req() ::
  #{
    oper => string(),
    data => term()
  }.

%% 处理请求
-spec handle_req(Req :: req()) -> {Oper :: string(), Data :: term()}.
handle_req(Req) ->
  {maps:get(oper, Req), maps:get(data, Req)}.

-spec login(Name :: string(), Pass :: string(), Role :: string()) -> binary().
login(Name, Pass, Role) ->
  Req = #login_request{data = #base_info{name = Name, pass = Pass, role = Role}},
  Oper = game_client_pb:enum_to_int(operation, Req#login_request.oper),
  Bin = list_to_binary(game_client_pb:encode_login_request(Req)),
  <<Oper, Bin/binary>>.

-spec register(Name :: string(), Pass :: string(), Role :: string()) -> binary().
register(Name, Pass, Role) ->
  Req = #register_request{data = #base_info{name = Name, pass = Pass, role = Role}},
  Oper = game_client_pb:enum_to_int(operation, Req#register_request.oper),
  Bin = list_to_binary(game_client_pb:encode_register_request(Req)),
  <<Oper, Bin/binary>>.

-spec login_out(Name :: string(), Pass :: string(), Role :: string()) -> binary().
login_out(Name, Pass, Role) ->
  Req = #login_out_request{data = #base_info{name = Name, pass = Pass, role = Role}},
  Oper = game_client_pb:enum_to_int(operation, Req#login_out_request.oper),
  Bin = list_to_binary(game_client_pb:encode_login_out_request(Req)),
  <<Oper, Bin/binary>>.

-spec upgrade() -> binary().
upgrade() ->
  Req = #upgrade_request{data = #empty{}},
  Oper = game_client_pb:enum_to_int(operation, Req#upgrade_request.oper),
  Bin = list_to_binary(game_client_pb:encode_upgrade_request(Req)),
  <<Oper, Bin/binary>>.

-spec change_name(New_name :: string()) -> binary().
change_name(New_name) ->
  Req = #change_name_request{data = #change_name_request_change_name{new_name = New_name}},
  Oper = game_client_pb:enum_to_int(operation, Req#change_name_request.oper),
  Bin = list_to_binary(game_client_pb:encode_change_name_request(Req)),
  <<Oper, Bin/binary>>.

-spec change_pass(New_pass :: string()) -> binary().
change_pass(New_pass) ->
  Req = #change_pass_request{data = #change_pass_request_change_pass{new_pass = New_pass}},
  Oper = game_client_pb:enum_to_int(operation, Req#change_pass_request.oper),
  Bin = list_to_binary(game_client_pb:encode_change_pass_request(Req)),
  <<Oper, Bin/binary>>.

-spec change_role(New_role :: string()) -> binary().
change_role(New_role) ->
  Req = #change_role_request{data = #change_role_request_change_role{new_role = New_role}},
  Oper = game_client_pb:enum_to_int(operation, Req#change_role_request.oper),
  Bin = list_to_binary(game_client_pb:encode_change_role_request(Req)),
  <<Oper, Bin/binary>>.

-spec te(Name :: string(), Lv :: integer(), Logo :: integer()) -> term().
te(Name, Lv, Logo) ->
  #te{name = Name, lv = Lv, logo = Logo}.

-spec add_tools(Tools :: list(#te{})) -> binary().
add_tools(Tools) ->
  Req = #add_tools_request{data = Tools},
  Oper = game_client_pb:enum_to_int(operation, Req#add_tools_request.oper),
  Bin = list_to_binary(game_client_pb:encode_add_tools_request(Req)),
  <<Oper, Bin/binary>>.

-spec delete_tools(Tool :: #te{}) -> binary().
delete_tools(Tool) ->
  Req = #delete_tools_request{data = Tool},
  Oper = game_client_pb:enum_to_int(operation, Req#delete_tools_request.oper),
  Bin = list_to_binary(game_client_pb:encode_delete_tools_request(Req)),
  <<Oper, Bin/binary>>.

-spec upgrade_tools(Tool :: #te{}) -> binary().
upgrade_tools(Tool) ->
  Req = #upgrade_tools_request{data = Tool},
  Oper = game_client_pb:enum_to_int(operation, Req#upgrade_tools_request.oper),
  Bin = list_to_binary(game_client_pb:encode_upgrade_tools_request(Req)),
  <<Oper, Bin/binary>>.

-spec add_equips(Equips :: list(#te{})) -> binary().
add_equips(Equips) ->
  Req = #add_equips_request{data = Equips},
  Oper = game_client_pb:enum_to_int(operation, Req#add_equips_request.oper),
  Bin = list_to_binary(game_client_pb:encode_add_equips_request(Req)),
  <<Oper, Bin/binary>>.

-spec delete_equips(Equip :: #te{}) -> binary().
delete_equips(Equip) ->
  Req = #delete_equips_request{data = Equip},
  Oper = game_client_pb:enum_to_int(operation, Req#delete_equips_request.oper),
  Bin = list_to_binary(game_client_pb:encode_delete_equips_request(Req)),
  <<Oper, Bin/binary>>.

-spec upgrade_equips(Equip :: #te{}) -> binary().
upgrade_equips(Equip) ->
  Req = #upgrade_equips_request{data = Equip},
  Oper = game_client_pb:enum_to_int(operation, Req#upgrade_equips_request.oper),
  Bin = list_to_binary(game_client_pb:encode_upgrade_equips_request(Req)),
  <<Oper, Bin/binary>>.

-spec info() -> binary().
info() ->
  Req = #info_request{data = #empty{}},
  Oper = game_client_pb:enum_to_int(operation, Req#info_request.oper),
  Bin = list_to_binary(game_client_pb:encode_info_request(Req)),
  <<Oper, Bin/binary>>.

-spec send_msg(From :: string(), Target :: string(), Msg :: string()) -> binary().
send_msg(From, Target, Msg) ->
  Req = #send_msg_request{data = #send_msg_request_message{from = From, target = Target, msg = Msg}},
  Oper = game_client_pb:enum_to_int(operation, Req#send_msg_request.oper),
  Bin = list_to_binary(game_client_pb:encode_send_msg_request(Req)),
  <<Oper, Bin/binary>>.