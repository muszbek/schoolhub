%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 13 May 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(course_manage_students_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TEST_USER_OWNER, <<"test_user_teacher">>).
-define(TEST_USER_STUDENT, <<"test_user_student">>).
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
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_OWNER, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_STUDENT, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':set_user_privilege(?ADMIN, ?TEST_USER_OWNER, <<"teacher">>),
    ok = 'Elixir.Schoolhub.CourseAdminServer':create_course(?TEST_USER_OWNER, ?TEST_COURSE),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    'Elixir.Schoolhub.CourseAdminServer':remove_course(?ADMIN, ?TEST_COURSE),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_STUDENT),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_OWNER),
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
    'Elixir.Schoolhub.CourseAdminServer':remove_student(?ADMIN, ?TEST_USER_STUDENT, ?TEST_COURSE),
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
    [{course_invite_students, [shuffle],
      [owner_invite_student_succeeds,
       admin_invite_student_succeeds,
       invite_invited_succeeds,
       invite_non_existing_student_fails,
       invite_wrong_course_fails,
       student_invite_fails]},
     {course_remove_students, [shuffle],
      [owner_remove_student_succeeds,
       admin_remove_student_succeeds,
       remove_removed_succeeds,
       remove_from_wrong_course_fails,
       student_remove_fails]},
     {course_set_affiliation, [shuffle],
      [owner_set_affiliation_succeeds,
       admin_set_affiliation_succeeds,
       change_affiliation_wrong_course_fails,
       change_wrong_affiliation_fails,
       change_affiliation_wrong_student_fails,
       student_change_affiliation_fails]},
     {course_get_all_affiliation, [shuffle],
      [owner_get_all_affiliation_succeeds,
       admin_get_all_affiliation_succeeds,
       get_all_affiliation_wrong_course_fails,
       student_get_all_affiliation_fails]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, course_invite_students},
     {group, course_remove_students},
     {group, course_set_affiliation},
     {group, course_get_all_affiliation}].

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

owner_invite_student_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

admin_invite_student_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

invite_invited_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

invite_non_existing_student_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_WRONG, 
							      ?TEST_COURSE),
    <<"ERROR_user_not_exist">> = Result,
    ok.

invite_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

student_invite_fails(_Config) ->
    'Elixir.Schoolhub.CourseAdminServer':invite_student(?ADMIN, ?TEST_USER_STUDENT, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':invite_student(?ADMIN, 
							      ?TEST_COURSE),
    <<"ERROR_no_permission">> = Result,
    ok.


owner_remove_student_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT,
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':remove_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

admin_remove_student_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':remove_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

remove_removed_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':remove_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

remove_from_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':remove_student(?TEST_USER_STUDENT, 
							      ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

student_remove_fails(_Config) ->
    'Elixir.Schoolhub.CourseAdminServer':invite_student(?ADMIN, ?TEST_USER_STUDENT, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':remove_student(?TEST_USER_OWNER, 
							      ?TEST_COURSE),
    <<"ERROR_no_permission">> = Result,
    ok.


owner_set_affiliation_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':set_affiliation(?TEST_USER_STUDENT, 
							       ?TEST_COURSE, <<"assistant">>),
    <<"ok">> = Result,
    ok.

admin_set_affiliation_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':set_affiliation(?TEST_USER_STUDENT, 
							       ?TEST_COURSE, <<"assistant">>),
    <<"ok">> = Result,
    ok.

change_affiliation_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':set_affiliation(?TEST_USER_STUDENT, 
							       ?TEST_COURSE_WRONG, 
							       <<"assistant">>),
    <<"ERROR_course_not_exist">> = Result,
    ok.

change_wrong_affiliation_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':set_affiliation(?TEST_USER_STUDENT, 
							       ?TEST_COURSE, <<"wrong_aff">>),
    <<"ERROR_wrong_affiliation">> = Result,
    ok.

change_affiliation_wrong_student_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':set_affiliation(?TEST_USER_WRONG, 
							       ?TEST_COURSE, <<"assistant">>),
    <<"ERROR_user_not_affiliated">> = Result,
    ok.

student_change_affiliation_fails(_Config) ->
    'Elixir.Schoolhub.CourseAdminServer':invite_student(?ADMIN, ?TEST_USER_STUDENT, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':set_affiliation(?TEST_USER_OWNER, 
							       ?TEST_COURSE, <<"assistant">>),
    <<"ERROR_no_permission">> = Result,
    ok.


owner_get_all_affiliation_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':get_all_affiliation(?TEST_COURSE),
    [[?TEST_USER_OWNER, <<"owner">>], [?TEST_USER_STUDENT, <<"student">>]] = Result,
    ok.

admin_get_all_affiliation_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    <<"ok">> = 'Elixir.Client.CourseAdminServer':invite_student(?TEST_USER_STUDENT, 
								?TEST_COURSE),
    Result = 'Elixir.Client.CourseAdminServer':get_all_affiliation(?TEST_COURSE),
    [[?TEST_USER_OWNER, <<"owner">>], [?TEST_USER_STUDENT, <<"student">>]] = Result,
    ok.

get_all_affiliation_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.CourseAdminServer':get_all_affiliation(?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

student_get_all_affiliation_fails(_Config) ->
    'Elixir.Schoolhub.CourseAdminServer':invite_student(?ADMIN, ?TEST_USER_STUDENT, ?TEST_COURSE),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseAdminServer':get_all_affiliation(?TEST_COURSE),
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
