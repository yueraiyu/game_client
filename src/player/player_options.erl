%%%-------------------------------------------------------------------
%%% @author dhcd
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十一月 2018 下午1:40
%%%-------------------------------------------------------------------
-module(player_options).
-author("dhcd").

%% define type
-export_type([player/0]).

-type player() ::
  #{
      index => atom(),
      pass  => string(),
      role  => atom(),
      base  => baseInfo(),
      bag   => bag()
    }.

-type baseInfo() ::
  #{
      name  => atom(),
      lv    => integer(),
      logo  => integer()
    }.

-type toolsOrEquips() ::
  #{
      count   => integer(),
      details => list(baseInfo())
  }.

-type bag() ::
  #{
      tools   => toolsOrEquips(),
      equips  => toolsOrEquips()
  }.

%% player options API
-export([init_base_info/3, init_tools_or_equips/2, init_bag/2]).

%% 初始化基本信息
-spec init_base_info(string(), integer(), integer()) -> player_options:baseInfo().
init_base_info(Name, Lv, Logo) -> #{name => Name, lv => Lv, logo => Logo}.

%% 初始化 装备 及 道具
-spec init_tools_or_equips(integer(), list(player_options:baseInfo())) -> player_options:toolsOrEquips().
init_tools_or_equips(Size, List) -> #{count => Size, details => List}.

%% 初始化 背包
-spec init_bag(Tools :: toolsOrEquips(), Equips :: toolsOrEquips()) -> bag().
init_bag(Tools, Equips) -> #{tools => Tools, equips => Equips}.


