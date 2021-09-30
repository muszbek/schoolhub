defmodule SchoolhubWeb.ChatView do
  use SchoolhubWeb, :view

  def get_host() do
    Application.get_env(:schoolhub, :xmpp)[:host]
  end
  
  def get_protocol() do
    Application.get_env(:schoolhub, :xmpp)[:protocol]
  end

  def get_domain() do
    [host: domain] = Application.get_env(:schoolhub, SchoolhubWeb.Endpoint)[:url]
    domain
  end

  def get_port() do
    Application.get_env(:schoolhub, :xmpp)[:port]
  end
end
