%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created :  1 Jun 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(course_content_SUITE).

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
-define(TEST_DESC, #{<<"text">> => <<"test">>}).
-define(TEST_DESC_MATCH, #{<<"text">> := <<"test">>}).
-define(TEST_DESC_STRING, <<"{\"text\":\"test\"}">>).

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
    [{course_description, [shuffle],
      [owner_set_desc_as_map_succeeds,
       owner_set_desc_as_string_succeeds,
       admin_set_desc_succeeds,
       student_set_desc_fails,
       student_get_desc_succeeds,
       set_desc_on_wrong_course_fails]},

     {course_message_post, [shuffle],
      [student_post_message_succeeds,
       not_affiliated_post_message_fails,
       student_post_message_wrong_course_fails,
       not_affiliated_get_single_message_fails,
       student_get_single_message_wrong_course_fails,
       student_get_single_message_wrong_id_fails]},

     {course_message_reply, [shuffle],
      [student_post_reply_succeeds,
       not_affiliated_post_reply_fails,
       student_post_reply_wrong_course_fails,
       student_post_reply_wrong_id_fails]},

     {course_message_delete, [shuffle],
      [teacher_delete_single_message_succeeds,
       teacher_delete_deleted_single_message_succeeds,
       admin_delete_single_message_succeeds,
       student_author_delete_single_message_succeeds,
       student_non_author_delete_single_message_fails,
       not_affiliated_delete_single_message_fails,
       wrong_course_delete_single_message_fails]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, course_description},
     {group, course_message_post},
     {group, course_message_reply},
     {group, course_message_delete}].

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
owner_set_desc_as_map_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_description(?TEST_COURSE, ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':get_description(?TEST_COURSE),
    ?TEST_DESC_MATCH = Result,
    ok.

owner_set_desc_as_string_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_description(?TEST_COURSE, 
								   ?TEST_DESC_STRING),
    Result = 'Elixir.Client.CourseContentServer':get_description(?TEST_COURSE),
    ?TEST_DESC_MATCH = Result,
    ok.

admin_set_desc_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_description(?TEST_COURSE, ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':get_description(?TEST_COURSE),
    ?TEST_DESC_MATCH = Result,
    ok.

student_set_desc_fails(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':set_description(?TEST_COURSE, ?TEST_DESC),
    <<"ERROR_no_permission">> = Result,
    ok.

student_get_desc_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    <<"ok">> = 'Elixir.Client.CourseContentServer':set_description(?TEST_COURSE, ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_description(?TEST_COURSE),
    ?TEST_DESC_MATCH = Result,
    ok.

set_desc_on_wrong_course_fails(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':set_description(?TEST_COURSE_WRONG, 
								 ?TEST_DESC),
    <<"ERROR_course_not_exist">> = Result,
    ok.


student_post_message_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':get_single_message(Id, ?TEST_COURSE),
    #{<<"message">> := ?TEST_DESC_MATCH} = Result,
    ok.

not_affiliated_post_message_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_NON_AFF, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
							      ?TEST_DESC),
    <<"ERROR_no_affiliation">> = Result,
    ok.

student_post_message_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE_WRONG,
							      ?TEST_DESC),
    <<"ERROR_course_not_exist">> = Result,
    ok.

not_affiliated_get_single_message_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_NON_AFF, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_single_message(1, ?TEST_COURSE),
    <<"ERROR_no_affiliation">> = Result,
    ok.

student_get_single_message_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_single_message(1, ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

student_get_single_message_wrong_id_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_single_message(0, ?TEST_COURSE),
    <<"ERROR_message_not_exist">> = Result,
    ok.


student_post_reply_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    #{<<"id">> := ReplyId} = 'Elixir.Client.CourseContentServer':post_reply(Id,
									    ?TEST_COURSE,
									    ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':get_single_message(ReplyId, ?TEST_COURSE),
    #{<<"message">> := ?TEST_DESC_MATCH} = Result,
    ok.

not_affiliated_post_reply_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_NON_AFF, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':post_reply(Id, ?TEST_COURSE, ?TEST_DESC),
    <<"ERROR_no_affiliation">> = Result,
    ok.

student_post_reply_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':post_reply(Id,
							    ?TEST_COURSE_WRONG,
							    ?TEST_DESC),
    <<"ERROR_course_not_exist">> = Result,
    ok.

student_post_reply_wrong_id_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := _Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':post_reply(0,
							    ?TEST_COURSE,
							    ?TEST_DESC),
    <<"ERROR_origin_not_exist">> = Result,
    ok.


teacher_delete_single_message_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(Id, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

teacher_delete_deleted_single_message_succeeds(_Config) ->
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(1, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

admin_delete_single_message_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(Id, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

student_author_delete_single_message_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(Id, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

student_non_author_delete_single_message_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT2, 
								?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(Id, ?TEST_COURSE),
    <<"ERROR_no_permission">> = Result,
    ok.

not_affiliated_delete_single_message_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_NON_AFF, 
								?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(Id, ?TEST_COURSE),
    <<"ERROR_no_affiliation">> = Result,
    ok.

wrong_course_delete_single_message_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    #{<<"id">> := Id} = 'Elixir.Client.CourseContentServer':post_message(?TEST_COURSE,
									 ?TEST_DESC),
    'Elixir.Client.LoginServer':end_session(),
    {ok, _OtherPid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_single_message(Id, 
								       ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
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
