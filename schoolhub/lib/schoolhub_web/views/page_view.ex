defmodule SchoolhubWeb.PageView do
  use SchoolhubWeb, :view

  alias Plug.Conn
  
  def get_server_name(conn) do
    server_name = Conn.fetch_cookies(conn)
    |> Map.from_struct()
    |> get_in([:cookies, "server-name"])
    
    case server_name do
      nil -> "Keep in touch with your students or teachers"
      name -> "You are on server \"" <> name <> "\""
    end
  end
end
