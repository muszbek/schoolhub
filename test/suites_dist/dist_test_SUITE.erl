%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(dist_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    start_apps(),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    stop_apps(),
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [parallel_auth].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
%my_test_case() -> 
%    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
parallel_auth(_Config) -> 
    Calls = async_call_all_clients({'Elixir.Client.Auth', auth, [<<"test_user">>, <<"test_pw">>]}),
    %timer:sleep(500),
    yield_all_clients(Calls, authenticated),
    ok.


%% Helper functions

start_apps() ->
    app_start_lib:start_elixir(),
    app_start_lib:start_server(),
    start_clients().

stop_apps() ->
    stop_clients(),
    application:stop(schoolhub).


start_clients() ->
    call_all_clients({app_start_lib, start_client_dist, []}, ok).

stop_clients() ->
    call_all_clients({application, stop, [schoolhub_client]}).


apply_to_all_clients(ApplyFun) ->
    ApplyToClient = fun(Node) ->
			    NodeStr = atom_to_list(Node),
			    case string:rstr(NodeStr, "client") of
				0 ->
				    false;
				I when is_integer(I) ->
				    ApplyFun(Node)
			    end
		    end,
    UnfilteredResult = lists:map(ApplyToClient, nodes()),
    lists:filter(fun(Elem) ->
			 Elem =/= false
		 end, UnfilteredResult).

call_all_clients({Module, Fun, Args}) ->
    ApplyFun = fun(Node) ->
		       rpc:call(Node, Module, Fun, Args)
	       end,

    apply_to_all_clients(ApplyFun).

call_all_clients({Module, Fun, Args}, ExpectedAnswer) ->
    ApplyFun = fun(Node) ->
		       ExpectedAnswer = rpc:call(Node, Module, Fun, Args)
	       end,
    
    apply_to_all_clients(ApplyFun).

async_call_all_clients({Module, Fun, Args}) ->
    ApplyFun = fun(Node) ->
		       rpc:async_call(Node, Module, Fun, Args)
	       end,
    
    apply_to_all_clients(ApplyFun).

yield_all_clients(AsyncCalls, ExpectedAnswer) ->
    YieldClient = fun(AsyncCall) ->
			  ExpectedAnswer = rpc:yield(AsyncCall)
		  end,

    lists:map(YieldClient, AsyncCalls).
    
