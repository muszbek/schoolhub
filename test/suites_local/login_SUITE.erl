%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(login_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TEST_USER, <<"test_user">>).
-define(TEST_PW, <<"test_pw">>).
-define(TEST_USER_WRONG, <<"test_usera">>).
-define(TEST_PW_WRONG, <<"test_pwa">>).

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
    'Elixir.Client.LoginServer':end_session(),
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
'Elixir.Client.LoginServer':end_session(),
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
    [{session_single_client, [shuffle], 
      [start_session_succeeds, session_already_started, session_auth_fails, session_no_user,
       end_session_succeeds, end_session_no_session]},
     {reg_single_client, [shuffle], 
      [reg_user_succeeds, reg_user_fails,
       remove_user_succeeds, remove_wrong_user_fails, remove_user_auth_fails]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, session_single_client},
     {group, reg_single_client}].

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

start_session_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
    ok.

session_already_started(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
    {error, {already_started, _Pid}} = 
	'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
    ok.

session_auth_fails(_Config) ->
    {error, "stored_key_mismatch"} = 
	'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW_WRONG),
    ok.

session_no_user(_Config) ->
    {error, "unknown_user"} = 
	'Elixir.Client.LoginServer':start_session(?TEST_USER_WRONG, ?TEST_PW),
    ok.

end_session_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
    ok = 'Elixir.Client.LoginServer':end_session(),
    ok.

end_session_no_session(_Config) ->
    {error, not_found} = 'Elixir.Client.LoginServer':end_session(),
    ok.


reg_user_succeeds(_Config) ->
    <<"ok">> = 'Elixir.Client.LoginServer':reg_user(?TEST_USER_WRONG, ?TEST_PW),
    ok.

reg_user_fails(_Config) ->
    <<"ERROR_", Rest/binary>> = 'Elixir.Client.LoginServer':reg_user(<<"">>, ?TEST_PW),
    ok.

remove_user_succeeds(_Config) ->
    <<"ok">> = 'Elixir.Client.LoginServer':remove_user(?TEST_USER, ?TEST_PW),
    ok.

remove_wrong_user_fails(_Config) ->
    {error, "unknown_user"} = 
	'Elixir.Client.LoginServer':remove_user(?TEST_USER_WRONG, ?TEST_PW),
    ok.

remove_user_auth_fails(_Config) ->
    {error, "stored_key_mismatch"} = 
	'Elixir.Client.LoginServer':remove_user(?TEST_USER, ?TEST_PW_WRONG),
    ok.


%% Helper functions
%% Closures

start_apps() ->
    ElixirPath = ct:get_config(elixir_path),
    ServerPath = ct:get_config(server_path),
    ClientPath = ct:get_config(client_path),
    ServerConfigs = ct:get_config(server_configs),
    ClientConfigs = ct:get_config(client_configs),
    
    app_start_lib:start_elixir(ElixirPath),
    app_start_lib:start_server(ServerPath, ServerConfigs),
    app_start_lib:start_client(ClientPath, ClientConfigs).

stop_apps() ->
    application:stop(schoolhub_client),
    application:stop(schoolhub).
