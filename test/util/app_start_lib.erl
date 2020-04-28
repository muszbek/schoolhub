%%%-------------------------------------------------------------------
%%% @author tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%% @copyright (C) 2020, tmuszbek
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2020 by tmuszbek <tmuszbek@tmuszbek-VirtualBox>
%%%-------------------------------------------------------------------
-module(app_start_lib).

-include_lib("common_test/include/ct.hrl").

%% API
-export([start_elixir/0, start_server/0, start_client/0, start_client/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

start_elixir() ->
    ElixirPath = ct:get_config(elixir_path),
    start_elixir(ElixirPath).

start_server() ->
    ServerPath = ct:get_config(server_path),
    ServerConfigs = ct:get_config(server_configs),
    start_server(ServerPath, ServerConfigs).

start_client() ->
    ClientPath = ct:get_config(client_path),
    ClientConfigs = ct:get_config(client_configs),
    start_client(ClientPath, ClientConfigs).

start_client(RootPath, Configs) ->
    start_app(RootPath, schoolhub_client, Configs).


%%%===================================================================
%%% Internal functions
%%%===================================================================

start_elixir(RootPath) ->
    add_deps_to_path(RootPath),
    ok = maybe_start(compiler),
    ok = maybe_start(elixir).

start_server(RootPath, Configs) ->
    start_app(RootPath, schoolhub, Configs).


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
    ok = c:cd(RootPath),
    {0, _StdioStream} = invoke_mix(),
    ok = c:cd(Cwd).

invoke_mix() ->
    {0, _} = invoke_command("mix compile").
    
%% implementation for calling bash command from erlang found at link below:
%% https://stackoverflow.com/questions/27028486/how-to-execute-system-command-in-erlang-and-get-results-unreliable-oscmd-1/27047529
invoke_command(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
	{Port, {data, Bytes}} ->
	    get_data(Port, [Sofar|Bytes]);
	{Port, eof} ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    true
	    end,
	    ExitCode =
		receive
		    {Port, {exit_status, Code}} ->
			Code
		end,
	    {ExitCode, lists:flatten(Sofar)}
    end.

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
