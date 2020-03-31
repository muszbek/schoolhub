defmodule SchoolhubClient do
  @moduledoc false

  use Application
  
  def start(_type, _args) do
    Client.Supervisor.start_link()
  end
end
