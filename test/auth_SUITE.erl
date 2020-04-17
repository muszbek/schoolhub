%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(auth_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(ELIXIR_PATH, << "/usr/lib/elixir/lib/" >>).
-define(SERVER_PATH, << "../../../schoolhub_server" >>).
-define(CLIENT_PATH, << "../../../schoolhub_client" >>).

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
    io:format("~p", [Config]),
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
    [{auth_single_client, [shuffle], 
      [auth_succeeds, auth_fails, unknown_user]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, auth_single_client}].

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
auth_succeeds(_Config) -> 
    authenticated = 'Elixir.Client.Auth':auth(?TEST_USER, ?TEST_PW),
    ok.

auth_fails(_Config) ->
    {error, "stored_key_mismatch"} = 'Elixir.Client.Auth':auth(?TEST_USER, ?TEST_PW_WRONG),
    ok.

unknown_user(_Config) ->
    {error, "unknown_user"} = 'Elixir.Client.Auth':auth(?TEST_USER_WRONG, ?TEST_PW),
    ok.

auth_in_parallel(_Config) ->
    auth_in_loop(5).


%% Helper functions
%% Closures

start_apps() ->
    start_elixir(),
    start_app(?SERVER_PATH, schoolhub),
    start_app(?CLIENT_PATH, schoolhub_client).

start_elixir() ->
    ok = application:start(compiler),
    RootPath = ?ELIXIR_PATH,
    start_app_with_deps(RootPath, elixir).

start_app(RootPath, AppName) ->
    DepsPath = << RootPath/binary, "/_build/dev/lib/" >>,
    load_configs(RootPath),
    start_app_with_deps(DepsPath, AppName).

load_configs(RootPath) ->
    load_config(RootPath, <<"config.exs">>),
    load_config(RootPath, <<"test.exs">>).

load_config(RootPath, File) ->
    ConfigPath = << RootPath/binary, "/config/", File/binary >>,
    Config = 'Elixir.Config.Reader':'read!'(ConfigPath),
    application:set_env(Config).

start_app_with_deps(DepsPath, AppName) ->
    add_deps_to_path(DepsPath),
    {ok, _apps}  = application:ensure_all_started(AppName).

add_deps_to_path(DepsPath) ->
    {ok, DepsNames} = file:list_dir(DepsPath),
    DepsBin = lists:map(fun list_to_binary/1, DepsNames),
    ok = lists:foreach(fun(Dep) -> 
			       DepFullPath = << DepsPath/binary, Dep/binary, "/ebin/" >>,
			       DepString = binary_to_list(DepFullPath),
			       true = code:add_path(DepString)
		       end, DepsBin).

stop_apps() ->
    application:stop(schoolhub_client),
    application:stop(schoolhub).


%% Tests

auth_in_loop(0) ->
    ok;

auth_in_loop(ProcessAmount) ->
    spawn_link(fun authenticate/0),
    io:format("~p", [ProcessAmount]),
    auth_in_loop(ProcessAmount - 1).

authenticate() ->
    authenticated = 'Elixir.Client.Auth':auth(?TEST_USER, ?TEST_PW).
