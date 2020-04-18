%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(app_start_lib).


-define(ELIXIR_PATH, << "/usr/lib/elixir/lib/" >>).
-define(SERVER_PATH, << "../../../schoolhub_server" >>).
-define(CLIENT_PATH, << "../../../schoolhub_client" >>).

%% API
-export([start_elixir/0, start_server/0, start_client/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start_elixir() ->
    ok = start_compiler(),
    RootPath = ?ELIXIR_PATH,
    start_app_with_deps(RootPath, elixir).

start_server() ->
    start_app(?SERVER_PATH, schoolhub).

start_client() ->
    start_app(?CLIENT_PATH, schoolhub_client).


%%%===================================================================
%%% Internal functions
%%%===================================================================

start_compiler() ->
    Apps = application:loaded_applications(),
    CompilerStarted = lists:keyfind(compiler, 1, Apps),
    io:format("Compiler: ~p", [CompilerStarted]),
    case CompilerStarted of
	{compiler, _, _} ->
	    ok;
	false ->
	    ok = application:start(compiler)
    end.

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
