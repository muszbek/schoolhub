defmodule Schoolhub do
  @moduledoc false

  use Application

  def start(_type, _args) do
    Schoolhub.Supervisor.start_link()
  end
  
end
