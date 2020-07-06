%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 21 Jun 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(course_message_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TEST_USER_OWNER, <<"test_user_teacher">>).
-define(TEST_USER_STUDENT, <<"test_user_student">>).
-define(TEST_USER_NON_AFF, <<"test_user">>).
-define(TEST_PW, <<"test_pw">>).
-define(ADMIN, <<"admin">>).
-define(ADMIN_PW, <<"admin">>).
-define(TEST_COURSE, <<"test_course">>).
-define(TEST_COURSE_WRONG, <<"test_course_wrong">>).
-define(TEST_DESC, #{<<"text">> => <<"test">>}).

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
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_NON_AFF, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':set_user_privilege(?ADMIN, ?TEST_USER_OWNER, <<"teacher">>),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
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
init_per_group(GroupName, Config) ->
    ok = 'Elixir.Schoolhub.CourseAdminServer':create_course(?TEST_USER_OWNER, ?TEST_COURSE),
    timer:sleep(500),
    ok = 'Elixir.Schoolhub.CourseAdminServer':invite_student(?TEST_USER_OWNER,
							     ?TEST_USER_STUDENT, ?TEST_COURSE),
    {ok, RootId} = 'Elixir.Schoolhub.CourseContentServer':post_message(?TEST_USER_STUDENT,
								       ?TEST_COURSE, ?TEST_DESC),
    {ok, ReplyId} = 'Elixir.Schoolhub.CourseContentServer':post_reply(RootId, ?TEST_USER_STUDENT,
								      ?TEST_COURSE, ?TEST_DESC),
    Config ++ [{root_id, RootId}, {reply_id, ReplyId}, {group, GroupName}].

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    'Elixir.Schoolhub.CourseAdminServer':remove_course(?ADMIN, ?TEST_COURSE),
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
    Group = ?config(group, Config),
    case Group of
	delete_root_message ->
	    {ok, RootId} = 
		'Elixir.Schoolhub.CourseContentServer':post_message(?TEST_USER_STUDENT,
								    ?TEST_COURSE, ?TEST_DESC),
	    {ok, ReplyId} = 
		'Elixir.Schoolhub.CourseContentServer':post_reply(RootId, ?TEST_USER_STUDENT,
								  ?TEST_COURSE, ?TEST_DESC),
	    Config ++ [{root_id, RootId}, {reply_id, ReplyId}];
	_ ->
	    Config
    end.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, Config) ->
    'Elixir.Client.LoginServer':end_session(),

    Group = ?config(group, Config),
    case Group of
	delete_root_message ->
	    RootId = ?config(root_id, Config),
	    ok = 'Elixir.Schoolhub.CourseContentServer':delete_root_message(RootId,
										  ?TEST_USER_OWNER,
										  ?TEST_COURSE);
	_ -> ok
    end,

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
    [{root_messages, [shuffle],
      [student_get_root_messages_succeeds,
       not_affiliated_get_root_messages_fails,
       get_root_messages_wrong_course_fails]},

     {message_replies, [shuffle],
      [student_get_replies_succeeds,
       not_affiliated_get_replies_fails,
       get_replies_wrong_course_fails,
       get_replies_wrong_id_returns_empty]},

     {pin_message, [shuffle],
      [teacher_pin_message_succeeds,
       admin_pin_message_succeeds,
       student_pin_message_fails,
       pin_message_wrong_course_fails,
       pin_message_wrong_id_fails,
       pin_reply_fails]},

     {delete_root_message, [shuffle],
      [teacher_delete_root_message_succeeds,
       admin_delete_root_message_succeeds,
       student_delete_root_message_fails,
       delete_root_message_wrong_course_fails,
       delete_non_existing_root_message_succeeds,
       delete_reply_succeeds]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, root_messages},
     {group, message_replies},
     {group, pin_message},
     {group, delete_root_message}].

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
student_get_root_messages_succeeds(_Config) -> 
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_root_messages(?TEST_COURSE),
    [#{<<"replies">> := 1}] = Result,
    ok.

not_affiliated_get_root_messages_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_NON_AFF, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_root_messages(?TEST_COURSE),
    <<"ERROR_no_affiliation">> = Result,
    ok.

get_root_messages_wrong_course_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_root_messages(?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.


student_get_replies_succeeds(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':get_replies(RootId, ?TEST_COURSE),
    [#{<<"id">> := RootId, <<"ancestor">> := nil}, #{<<"ancestor">> := RootId}] = Result,
    ok.

not_affiliated_get_replies_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_NON_AFF, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':get_replies(RootId, ?TEST_COURSE),
    <<"ERROR_no_affiliation">> = Result,
    ok.

get_replies_wrong_course_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':get_replies(RootId, ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

get_replies_wrong_id_returns_empty(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':get_replies(0, ?TEST_COURSE),
    [] = Result,
    ok.


teacher_pin_message_succeeds(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':pin_message(RootId, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

admin_pin_message_succeeds(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':pin_message(RootId, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

student_pin_message_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':pin_message(RootId, ?TEST_COURSE),
    <<"ERROR_no_permission">> = Result,
    ok.

pin_message_wrong_course_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':pin_message(RootId, ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

pin_message_wrong_id_fails(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':pin_message(0, ?TEST_COURSE),
    <<"ERROR_message_not_exist">> = Result,
    ok.

pin_reply_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    ReplyId = ?config(reply_id, Config),
    Result = 'Elixir.Client.CourseContentServer':pin_message(ReplyId, ?TEST_COURSE),
    <<"ERROR_message_not_exist">> = Result,
    ok.


teacher_delete_root_message_succeeds(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':delete_root_message(RootId, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

admin_delete_root_message_succeeds(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?ADMIN, ?ADMIN_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':delete_root_message(RootId, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

student_delete_root_message_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_STUDENT, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':delete_root_message(RootId, ?TEST_COURSE),
    <<"ERROR_no_permission">> = Result,
    ok.

delete_root_message_wrong_course_fails(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    RootId = ?config(root_id, Config),
    Result = 'Elixir.Client.CourseContentServer':delete_root_message(RootId, 
								     ?TEST_COURSE_WRONG),
    <<"ERROR_course_not_exist">> = Result,
    ok.

delete_non_existing_root_message_succeeds(_Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    Result = 'Elixir.Client.CourseContentServer':delete_root_message(0, ?TEST_COURSE),
    <<"ok">> = Result,
    ok.

delete_reply_succeeds(Config) ->
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER_OWNER, ?TEST_PW),
    ReplyId = ?config(reply_id, Config),
    Result = 'Elixir.Client.CourseContentServer':delete_root_message(ReplyId, ?TEST_COURSE),
    <<"ok">> = Result,
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
