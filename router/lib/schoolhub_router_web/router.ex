defmodule SchoolhubRouterWeb.Router do
  use SchoolhubRouterWeb, :router

  import SchoolhubRouterWeb.Plugs
  
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

  pipeline :admin do
    plug :authorize_admin
  end
  

  scope "/router", SchoolhubRouterWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/phoenix", PageController, :phoenix
    get "/description", DescriptionController, :index
    get "/contact", DescriptionController, :contact
    
    get "/servers/unsubscribe", ServerController, :unsubscribe
    post "/servers/unsubscribe", ServerController, :email_unsubscribe
    get "/servers/unsubscribe/:token", ServerController, :token_unsubscribe
    resources "/servers", ServerController, only: [:index, :show, :new, :create]
    post "/redirect", ServerController, :to_instance

    get "/admin_authorization", AdminController, :index
    post "/admin_authorization", AdminController, :verify
  end

  scope "/router/admin_panel", SchoolhubRouterWeb do
    pipe_through :browser
    pipe_through :admin
    
    get "/", AdminController, :panel
    post "/servers", AdminController, :create
    post "/unsubscribe", AdminController, :admin_unsubscribe
  end

  scope "/router/admin_pw", SchoolhubRouterWeb do
    pipe_through :api

    get "/:pod_name", ServerController, :get_admin_pw
  end

  # Other scopes may use custom stacks.
  # scope "/api", SchoolhubRouterWeb do
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
      live_dashboard "/dashboard", metrics: SchoolhubRouterWeb.Telemetry
    end
  end
end
