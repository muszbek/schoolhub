defmodule SchoolhubClient do
  @moduledoc false

  use Application
  
  def start(_type, _args) do
    Client.Supervisor.start_link()
  end

  def register(username, password) do
    Client.LoginServer.reg_user(username, password)
  end

  def login(username, password) do
    Client.LoginServer.start_session(username, password)
  end

  def logout() do
    Client.LoginServer.end_session()
  end
  
end
