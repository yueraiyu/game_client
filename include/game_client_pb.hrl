-ifndef(GAME_CLIENT_PB_H).
-define(GAME_CLIENT_PB_H, true).
-record(game_client, {
    uid = erlang:error({required, uid}),
    name,
    leave = [],
    equ_name = [],
    equ_leave,
    tools_name,
    tool_num
}).
-endif.

