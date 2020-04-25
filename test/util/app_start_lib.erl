%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(app_start_lib).


%% API
-export([start_elixir/1, start_server/1, start_client/1, start_client/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start_elixir(RootPath) ->
    add_deps_to_path(RootPath),
    ok = maybe_start(compiler),
    ok = maybe_start(elixir).

start_server(RootPath) ->
    start_app(RootPath, schoolhub).

start_client(RootPath) ->
    start_app(RootPath, schoolhub_client).

start_client(RootPath, Configs) ->
    start_app(RootPath, schoolhub_client, Configs).


%%%===================================================================
%%% Internal functions
%%%===================================================================

add_deps_to_path(DepsPath) ->
    {ok, DepsNames} = file:list_dir(DepsPath),
    DepsBin = lists:map(fun list_to_binary/1, DepsNames),
    ok = lists:foreach(fun(Dep) -> 
			       DepFullPath = << DepsPath/binary, Dep/binary, "/ebin/" >>,
			       DepString = binary_to_list(DepFullPath),
			       true = code:add_path(DepString)
		       end, DepsBin).

maybe_start(AppName) ->
    Apps = application:loaded_applications(),
    CheckStarted = lists:keyfind(AppName, 1, Apps),
    case CheckStarted of
	{AppName, _, _} ->
	    ok;
	false ->
	    ok = application:start(AppName)
    end.

start_app(RootPath, AppName) ->
    start_app(RootPath, AppName, []).

start_app(RootPath, AppName, Configs) ->
    compile_app(RootPath),
    DepsPath = << RootPath/binary, "/_build/dev/lib/" >>,
    load_configs(RootPath, Configs),
    add_deps_to_path(DepsPath),
    {ok, _Apps} = application:ensure_all_started(AppName),
    ok.

compile_app(RootPath) ->
    {ok, Cwd} = file:get_cwd(),
    ok = cd(RootPath),
    _ = os:cmd("mix compile"),
    ok = cd(Cwd).

load_configs(RootPath, []) ->
    load_configs(RootPath, [<<"config.exs">>, <<"test.exs">>]);

load_configs(RootPath, Configs) ->
    lists:map(fun(Config) ->
		      load_config(RootPath, Config)
	      end, Configs).

load_config(RootPath, File) ->
    ConfigPath = << RootPath/binary, "/config/", File/binary >>,
    Config = 'Elixir.Config.Reader':'read!'(ConfigPath),
    application:set_env(Config).
