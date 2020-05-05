%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(chat_archive_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(TEST_USER, <<"test_user">>).
-define(TEST_PW, <<"test_pw">>).
-define(TEST_USER_NEW, <<"test_user_new">>).
-define(TEST_USER_WRONG, <<"test_user_non_existent">>).
-define(TEST_MESSAGE, <<"The quick brown fox jumps over the lazy dog.">>).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,60}}].

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
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER, ?TEST_PW),
    'Elixir.Schoolhub.RegServer':register_user(?TEST_USER_NEW, ?TEST_PW),
    timer:sleep(100),
    {ok, _Pid} = 'Elixir.Client.LoginServer':start_session(?TEST_USER, ?TEST_PW),
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
    {ok, _} = 'Elixir.Schoolhub.RegServer':purge_user(?TEST_USER),
    {ok, _} = 'Elixir.Schoolhub.RegServer':purge_user(?TEST_USER_NEW),
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
    [{mam, [shuffle], [empty_archive_returns_empty,
		       get_archive_succeeds,
		       get_more_archive_does_not_bring_more,
		       message_after_archive_is_recorded,
		       get_unknown_archive_returns_empty]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, mam}].

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
empty_archive_returns_empty(_Config) -> 
    [] = 'Elixir.Client.ChatArchiveServer':get_archive(?TEST_USER_NEW),
    ok.

get_archive_succeeds(_Config) ->
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    Archive = 'Elixir.Client.ChatArchiveServer':get_archive(?TEST_USER_NEW),
    [[<<"O">>, ?TEST_MESSAGE], [<<"O">>, ?TEST_MESSAGE]] = Archive,
    ok.

get_more_archive_does_not_bring_more(_Config) ->
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    Archive = 'Elixir.Client.ChatArchiveServer':get_archive(?TEST_USER_NEW),
    [[<<"O">>, ?TEST_MESSAGE], [<<"O">>, ?TEST_MESSAGE]] = Archive,
    Archive = 'Elixir.Client.ChatArchiveServer':get_more_archive(?TEST_USER_NEW),
    Archive = 'Elixir.Client.ChatArchiveServer':get_more_archive(?TEST_USER_NEW),
    ok.

message_after_archive_is_recorded(_Config) ->
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    Archive = 'Elixir.Client.ChatArchiveServer':get_archive(?TEST_USER_NEW),
    [[<<"O">>, ?TEST_MESSAGE], [<<"O">>, ?TEST_MESSAGE]] = Archive,
    ok = 'Elixir.Client.ChatServer':chat(?TEST_USER_NEW, ?TEST_MESSAGE),
    NewArchive = 'Elixir.Client.ChatArchiveServer':get_archive(?TEST_USER_NEW),
    [[<<"O">>, ?TEST_MESSAGE], [<<"O">>, ?TEST_MESSAGE], [<<"O">>, ?TEST_MESSAGE]] = NewArchive,
    ok.

get_unknown_archive_returns_empty(_Config) ->
    [] = 'Elixir.Client.ChatArchiveServer':get_archive(?TEST_USER_WRONG),
    [] = 'Elixir.Client.ChatArchiveServer':get_more_archive(?TEST_USER_NEW),
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
