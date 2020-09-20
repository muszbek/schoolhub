defmodule SchoolhubWeb.Router do
  use SchoolhubWeb, :router
  
  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :session do
    plug :authenticate_user
  end

  scope "/", SchoolhubWeb do
    pipe_through :browser

    get "/", PageController, :index
    resources "/users", UserController, only: [:new, :create]
    resources "/sessions", SessionController, only: [:new, :create, :delete],
      singleton: true

    pipe_through :session
    resources "/users", UserController, except: [:new, :create]
    resources "/privileges", PrivilegeController, except: [:new, :create, :delete]
    resources "/courses", CourseController do
      resources "/affiliations", AffiliationController
    end
  end

  scope "/auth", SchoolhubWeb do
    pipe_through :api

    post "/", SessionController, :authenticate
  end

  # Other scopes may use custom stacks.
  # scope "/api", SchoolhubWeb do
  #   pipe_through :api
  # end

  # Enables LiveDashboard only for development
  #
  # If you want to use the LiveDashboard in production, you should put
  # it behind authentication and allow only admins to access it.
  # If your application does not have an admins-only section yet,
  # you can use Plug.BasicAuth to set up some basic authentication
  # as long as you are also using SSL (which you should anyway).
  if Mix.env() in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: SchoolhubWeb.Telemetry
    end
  end

  
  defp authenticate_user(conn, _) do
    case get_session(conn, :user_id) do
      nil ->
        redirect_to_login(conn)
      user_id ->
	try do
          assign(conn, :current_user, Schoolhub.Accounts.get_user!(user_id))
	rescue
	  Ecto.NoResultsError ->
	    redirect_to_login(conn)
	end
    end
  end

  defp redirect_to_login(conn) do
    conn
    |> Phoenix.Controller.put_flash(:error, "Login required")
    |> Phoenix.Controller.redirect(to: "/sessions/new")
    |> halt()
  end
end
