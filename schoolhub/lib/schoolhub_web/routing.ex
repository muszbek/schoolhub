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
    route(fun_name, conn, args, route_prefix)
  end

  def route(fun_name, conn, args, internal_host) do
    path = Kernel.apply(String.to_existing_atom(@routes_module), fun_name, [conn] ++ args)
    internal_host <> path
  end

  def internal_host(%Plug.Conn{} = conn) do
    host = get_req_header(conn, "x-internal-host")
    _route_prefix = internal_host(host)
  end
  def internal_host([]), do: ""
  def internal_host([hostname]), do: "/" <> hostname
  def internal_host(hostname) when is_binary(hostname), do: hostname
  
end
