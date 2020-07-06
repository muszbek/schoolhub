%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(course_grading_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TEST_USER_OWNER, <<"test_user_teacher">>).
-define(TEST_USER_STUDENT, <<"test_user_student">>).
-define(TEST_USER_STUDENT2, <<"test_user_student2">>).
-define(TEST_USER_NON_AFF, <<"test_user">>).
-define(TEST_PW, <<"test_pw">>).
-define(ADMIN, <<"admin">>).
-define(ADMIN_PW, <<"admin">>).
-define(TEST_COURSE, <<"test_course">>).
-define(TEST_COURSE_WRONG, <<"test_course_wrong">>).
-define(TEST_GRADE, #{<<"grade">> => 10}).
-define(TEST_GRADE_MATCH, #{<<"grade">> := 10}).
-define(TEST_GRADE_APPEND_BASE, #{<<"old_grade">> => 8, <<"grade">> => 9}).
-define(TEST_GRADE_APPEND, #{<<"grade">> => 10, <<"total">> => 18}).
-define(TEST_GRADE_APPEND_MATCH, #{<<"old_grade">> := 8, <<"grade">> := 10, <<"total">> := 18}).

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
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_OWNER, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_STUDENT, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_STUDENT2, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_NON_AFF, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':set_user_privilege(?ADMIN, ?TEST_USER_OWNER, <<"teacher">>),
    ok = 'Elixir.Schoolhub.CourseAdminServer':create_course(?TEST_USER_OWNER, ?TEST_COURSE),
    timer:sleep(500),
    ok = 'Elixir.Schoolhub.CourseAdminServer':invite_student(?TEST_USER_OWNER,
							     ?TEST_USER_STUDENT, 
							     ?TEST_COURSE),
    ok = 'Elixir.Schoolhub.CourseAdminServer':invite_student(?TEST_USER_OWNER,
							     ?TEST_USER_STUDENT2, 
							     ?TEST_COURSE),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    'Elixir.Schoolhub.CourseAdminServer':remove_course(?ADMIN, ?TEST_COURSE),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_STUDENT),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_STUDENT2),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_OWNER),
    {ok, _} = 'Elixir.Schoolhub.RegServer':remove_user(?TEST_USER_NON_AFF),
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
    [{course_grading_singular, [shuffle],
      [owner_set_grade_succeeds,
       admin_set_grade_succeeds,
       student_set_grade_fails,
       student_get_own_grade_succeeds,
       student_get_other_student_grade_fails,
       wrong_course_set_grade_fails,
       wrong_course_get_grade_fails,
       wrong_target_student_set_grade_fails,
       wrong_target_student_get_grade_fails]},

     {course_grading_singular_append, [shuffle],
      [teacher_append_grade_succeeds,
       admin_append_grade_succeeds,
       student_append_grade_fails,
       wrong_course_append_grade_fails,
       wrong_target_student_append_grade_fails]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, course_grading_singular},
     {group, course_grading_singular_append}].

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
owner_set_grade_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT,
							      ?TEST_GRADE),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_STUDENT),
    ?TEST_GRADE_MATCH = Result,
    ok.

admin_set_grade_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT,
							      ?TEST_GRADE),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_STUDENT),
    ?TEST_GRADE_MATCH = Result,
    ok.

student_set_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT,
							    ?TEST_GRADE),
    <<"ERROR_no_permission">> = Result,
    ok.

student_get_own_grade_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT,
							      ?TEST_GRADE),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_STUDENT),
    ?TEST_GRADE_MATCH = Result,
    ok.

student_get_other_student_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT2, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_STUDENT),
    <<"ERROR_no_permission">> = Result,
    ok.

wrong_course_set_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE_WRONG,
							    ?TEST_USER_STUDENT, ?TEST_GRADE),
    <<"ERROR_course_not_exist">> = Result,
    ok.

wrong_course_get_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE_WRONG, 
							    ?TEST_USER_STUDENT),
    <<"ERROR_course_not_exist">> = Result,
    ok.

wrong_target_student_set_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_NON_AFF, 
							    ?TEST_GRADE),
    <<"ERROR_no_affiliation">> = Result,
    ok.

wrong_target_student_get_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_NON_AFF),
    <<"ERROR_no_affiliation">> = Result,
    ok.


teacher_append_grade_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
							      ?TEST_GRADE_APPEND_BASE),
    <<"ok">> = 'Elixir.Client.CourseContentServer':append_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
								 ?TEST_GRADE_APPEND),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_STUDENT2),
    ?TEST_GRADE_APPEND_MATCH = Result,
    ok.

admin_append_grade_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
							      ?TEST_GRADE_APPEND_BASE),
    <<"ok">> = 'Elixir.Client.CourseContentServer':append_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
								 ?TEST_GRADE_APPEND),
    Result = 'Elixir.Client.CourseContentServer':get_grades(?TEST_COURSE, ?TEST_USER_STUDENT2),
    ?TEST_GRADE_APPEND_MATCH = Result,
    ok.

student_append_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
							      ?TEST_GRADE_APPEND_BASE),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':append_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
							       ?TEST_GRADE_APPEND),
    <<"ERROR_no_permission">> = Result,
    ok.

wrong_course_append_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
							      ?TEST_GRADE_APPEND_BASE),
    Result = 'Elixir.Client.CourseContentServer':append_grades(?TEST_COURSE_WRONG, 
							       ?TEST_USER_STUDENT2,
							       ?TEST_GRADE_APPEND),
    <<"ERROR_course_not_exist">> = Result,
    ok.

wrong_target_student_append_grade_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_grades(?TEST_COURSE, ?TEST_USER_STUDENT2,
							      ?TEST_GRADE_APPEND_BASE),
    Result = 'Elixir.Client.CourseContentServer':append_grades(?TEST_COURSE, ?TEST_USER_NON_AFF,
							       ?TEST_GRADE_APPEND),
    <<"ERROR_no_affiliation">> = Result,
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
