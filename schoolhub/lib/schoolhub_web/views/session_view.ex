defmodule SchoolhubWeb.SessionView do
  use SchoolhubWeb, :view

  def get_host() do
    Application.get_env(:schoolhub, :xmpp)[:host]
  end
  
  def get_protocol() do
    Application.get_env(:schoolhub, :xmpp)[:protocol]
  end

  def get_domain() do
    System.get_env("DOMAIN", "localhost")
  end

  def get_port() do
    Application.get_env(:schoolhub, :xmpp)[:port]
  end
end
