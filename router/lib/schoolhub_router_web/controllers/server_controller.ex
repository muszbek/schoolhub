defmodule SchoolhubRouterWeb.ServerController do
  use SchoolhubRouterWeb, :controller

  alias SchoolhubRouter.Instances
  alias SchoolhubRouter.Instances.Server

  @pod_address_suffix ".schoolhub.default.svc.cluster.local"
  
  def index(conn, _params) do
    servers = Instances.list_servers()
    render(conn, "index.html", servers: servers)
  end

  def new(conn, _params) do
    changeset = Instances.change_server(%Server{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"server" => server_params}) do
    case Instances.create_server_with_k8s(server_params) do
      {:ok, _server} ->
        conn
        |> put_flash(:info, "Server created successfully.")
        |> redirect(to: Routes.page_path(conn, :index))

      {:error, %K8s.Client.APIError{}} ->
	conn
        |> put_flash(:error, "Server cannot be created due to internal error.")
        |> redirect(to: Routes.server_path(conn, :new))

      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)

      {:error, _error} ->
	conn
        |> put_flash(:error, "Cannot connect to Kubernetes.")
        |> redirect(to: Routes.server_path(conn, :new))
    end
  end

  def show(conn, %{"id" => id}) do
    server = Instances.get_server!(id)
    render(conn, "show.html", server: server)
  end

  def to_instance(conn, %{"server_name" => server_name}) do
    case Instances.get_server_by_name(server_name) do
      nil ->
	conn
	|> put_flash(:error, "No server found under that name.")
	|> redirect(to: Routes.page_path(conn, :index))
      server ->
	path = "/" <> server.address <> "/"
	conn
	|> put_resp_cookie("server-name", server_name)
	|> redirect(to: path)
    end
  end

  def get_admin_pw(conn, %{"pod_name" => pod_name}) do
    address = pod_name <> @pod_address_suffix
    server = Instances.get_server_by_address(address)

    ## The first time somebody asks for the password
    ## (it can be only the corresponding server)
    ## delete the data
    Instances.update_server(server, %{admin_pw: nil})
    
    render(conn, "admin_pw.json", admin_pw: server.admin_pw)
  end

end
