defmodule SchoolhubRouterWeb.ServerController do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouter.Instances
  alias SchoolhubRouter.Instances.Server

  def index(conn, _params) do
    servers = Instances.list_servers()
    render(conn, "index.html", servers: servers)
  end

  def new(conn, _params) do
    changeset = Instances.change_server(%Server{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"server" => server_params}) do
    case Instances.create_server(server_params) do
      {:ok, server} ->
        conn
        |> put_flash(:info, "Server created successfully.")
        |> redirect(to: Routes.server_path(conn, :show, server))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    server = Instances.get_server!(id)
    render(conn, "show.html", server: server)
  end

end
