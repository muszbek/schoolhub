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
-define(ELIXIR_DOCKER_PATH, << "/usr/local/lib/elixir/lib/" >>).
-define(SERVER_PATH, << "../../../schoolhub_server" >>).
-define(CLIENT_PATH, << "../../../schoolhub_client" >>).
-define(SERVER_PATH_DIST, << "/root/schoolhub_server" >>).
-define(CLIENT_PATH_DIST, << "/root/schoolhub_client" >>).

%% API
-export([start_elixir/0, start_server/0, start_client/0, 
	 start_server_dist/0, start_client_dist/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start_elixir() ->
    RootPath = ?ELIXIR_PATH,
    start_elixir(RootPath).

start_elixir(RootPath) ->
    add_deps_to_path(RootPath),
    ok = maybe_start(compiler),
    ok = maybe_start(elixir).

start_server() ->
    start_app(?SERVER_PATH, schoolhub).

start_client() ->
    start_app(?CLIENT_PATH, schoolhub_client).

start_server_dist() ->
    start_elixir(?ELIXIR_DOCKER_PATH),
    start_app(?SERVER_PATH_DIST, schoolhub).

start_client_dist() ->
    start_elixir(?ELIXIR_DOCKER_PATH),
    start_app(?CLIENT_PATH_DIST, schoolhub_client, [<<"test_dist.exs">>]).


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
    DepsPath = << RootPath/binary, "/_build/dev/lib/" >>,
    load_configs(RootPath, Configs),
    add_deps_to_path(DepsPath),
    {ok, _Apps} = application:ensure_all_started(AppName),
    ok.

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
