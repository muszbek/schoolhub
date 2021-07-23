defmodule SchoolhubWeb.Routing do
  @moduledoc """
  A custom module to encapsulate SchoolhubWeb.Router.Helpers.
  I had to name it differently than "Routes" so it is easier to track the changes.
  Its purpose is to be able to dynamically prepend static paths with the internal host read from a HTTP request header.
  """

  import Plug.Conn
  
  @routes_module "Elixir.SchoolhubWeb.Router.Helpers"

  def route(fun_name, conn, args) do
    route_prefix = internal_host(conn)
    path = Kernel.apply(String.to_existing_atom(@routes_module), fun_name, [conn] ++ args)
    route_prefix <> path
  end

  def internal_host(conn) do
    internal_host = get_req_header(conn, "x-internal-host")
    _route_prefix = case internal_host do
		      [] -> ""
		      [hostname] -> "/" <> hostname
		    end
  end
  
end
