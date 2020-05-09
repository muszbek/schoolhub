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
-define(TEST_USER_WRONG, <<"test_user_wrong">>).
-define(TEST_PW_WRONG, <<"test_pwa">>).
-define(TEST_USER_NEW, <<"test_user_new">>).
-define(ADMIN, <<"admin">>).
-define(ADMIN_PW, <<"admin">>).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,120}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    start_apps(),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_NEW),
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER, ?TEST_PW),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_NEW),
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
       remove_user_succeeds, remove_wrong_user_fails, remove_user_auth_fails]},
     {change_privileges_client, [shuffle],
      [non_admin_change_privilege_fails,
       admin_change_privilege_succeeds,
       set_wrong_privilege_fails,
       change_privilege_on_non_existing_user_fails,
       set_self_privilege_fails]}].

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
     {group, reg_single_client},
     {group, change_privileges_client}].

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
    <<"ok">> = 'Elixir.Client.LoginServer':reg_user(?TEST_USER_NEW, ?TEST_PW),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_NEW),
    ok.

reg_user_fails(_Config) ->
    <<"ERROR_", _Rest/binary>> = 'Elixir.Client.LoginServer':reg_user(<<"">>, ?TEST_PW),
    ok.

remove_user_succeeds(_Config) ->
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_NEW, ?TEST_PW),
    timer:sleep(100),
    <<"ok">> = 'Elixir.Client.LoginServer':remove_user(?TEST_USER_NEW, ?TEST_PW),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_NEW),
    ok.

remove_wrong_user_fails(_Config) ->
    {error, "unknown_user"} = 
	'Elixir.Client.LoginServer':remove_user(?TEST_USER_WRONG, ?TEST_PW),
    ok.

remove_user_auth_fails(_Config) ->
    {error, "stored_key_mismatch"} = 
	'Elixir.Client.LoginServer':remove_user(?TEST_USER, ?TEST_PW_WRONG),
    ok.


get_privilege_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.AdminServer':get_privilege(),
    <<"admin">> = Result,
    ok.

non_admin_change_privilege_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
    Result = 'Elixir.Client.AdminServer':set_privilege(?ADMIN, <<"teacher">>),
    <<"ERROR_no_permission">> = Result,
    ok.

admin_change_privilege_succeeds(_Config) ->
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_NEW, ?TEST_PW),
    timer:sleep(500),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.AdminServer':set_privilege(?TEST_USER_NEW, <<"teacher">>),
    <<"ok">> = Result,
    ok.

set_wrong_privilege_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.AdminServer':set_privilege(?TEST_USER, <<"unknown">>),
    <<"ERROR_wrong_privilege">> = Result,
    ok.

change_privilege_on_non_existing_user_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.AdminServer':set_privilege(?TEST_USER_WRONG, <<"teacher">>),
    <<"ERROR_wrong_privilege">> = Result,
    ok.

set_self_privilege_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
    Result = 'Elixir.Client.AdminServer':set_privilege(?TEST_USER, <<"teacher">>),
    <<"ERROR_set_self_privilege">> = Result,
    ok.


%% Helper functions
%% Closures

start_apps() ->
    app_start_lib:start_elixir(),
    app_start_lib:start_server(),
    app_start_lib:start_client().

stop_apps() ->
    application:stop(schoolhub_client),
    application:stop(schoolhub).
