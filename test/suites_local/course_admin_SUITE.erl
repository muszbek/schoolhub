%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(course_admin_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TEST_USER_TEACHER, <<"test_user_teacher">>).
-define(TEST_USER_STUDENT, <<"test_user_new">>).
-define(TEST_PW, <<"test_pw">>).
-define(TEST_USER_WRONG, <<"test_user_wrong">>).
-define(ADMIN, <<"admin">>).
-define(ADMIN_PW, <<"admin">>).
-define(TEST_COURSE, <<"test_course">>).
-define(TEST_COURSE_WRONG, <<"test_course_wrong">>).

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
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_TEACHER, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':set_user_privilege(?ADMIN, ?TEST_USER_TEACHER, <<"teacher">>),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    'Elixir.Schoolhub.CourseServer':remove_course(?ADMIN, ?TEST_COURSE),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_STUDENT),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_TEACHER),
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
    'Elixir.Client.LoginServer':end_session(),
    'Elixir.Schoolhub.CourseServer':remove_course(?ADMIN, ?TEST_COURSE),
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
    [{course_admin_client, [shuffle],
      [teacher_create_course_succeeds,
       owner_get_affiliation_succeeds,
       not_affiliated_user_no_affiliation,
       affiliation_on_wrong_course_fails,
       owner_remove_course_succeeds,
       admin_remove_course_succeeds,
       remove_wrong_course_fails,
       student_remove_course_fails]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, course_admin_client}].

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
teacher_create_course_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_TEACHER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':create_course(?TEST_COURSE),
    <<"ok">> = Result,
    ok.

owner_get_affiliation_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_TEACHER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':create_course(?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':get_affiliation(?TEST_COURSE),
    <<"owner">> = Result,
    ok.

not_affiliated_user_no_affiliation(_Config) ->
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_STUDENT, ?TEST_PW),
    timer:sleep(500),
    ok = 'Elixir.Schoolhub.CourseServer':create_course(?TEST_USER_TEACHER, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':get_affiliation(?TEST_COURSE),
    <<"ERROR_no_affiliation">> = Result,
    ok.

affiliation_on_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_TEACHER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':get_affiliation(?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

owner_remove_course_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_TEACHER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':create_course(?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':remove_course(?TEST_COURSE),
    <<"ok">> = Result,
    ok.

admin_remove_course_succeeds(_Config) ->
    ok = 'Elixir.Schoolhub.CourseServer':create_course(?TEST_USER_TEACHER, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.CourseAdminServer':remove_course(?TEST_COURSE),
    <<"ok">> = Result,
    ok.

remove_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_TEACHER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':remove_course(?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

student_remove_course_fails(_Config) ->
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_STUDENT, ?TEST_PW),
    timer:sleep(500),
    ok = 'Elixir.Schoolhub.CourseServer':create_course(?TEST_USER_TEACHER, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':remove_course(?TEST_COURSE),
    <<"ERROR_no_permission">> = Result,
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
