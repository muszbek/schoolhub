defmodule Schoolhub do
  @moduledoc false

  use Application

  @config_root "./config/"

  def start(_type, _args) do
    maybe_read_config()
    
    Schoolhub.Supervisor.start_link()
  end

  
  defp maybe_read_config() do
    if is_test() do
      config = Config.Reader.read!(@config_root <> "test.exs")
      Application.put_all_env(config)
    end
  end

  defp is_test() do
    is_mix_started() and Mix.env == :test
  end
  
  defp is_mix_started() do
    apps = Application.started_applications()
    Enum.find_value(apps, false, fn(app) ->
      {name, _desc, _ver} = app
      name == :mix
    end)
  end
  
end
