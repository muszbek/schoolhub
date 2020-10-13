defmodule SchoolhubWeb.Router do
  use SchoolhubWeb, :router

  import SchoolhubWeb.Plugs
  
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

  pipeline :admin do
    plug :need_admin_priv
  end

  pipeline :teacher do
    plug :need_teacher_priv
  end

  pipeline :course_owner do
    plug :need_owner_aff
  end

  pipeline :course_assistant do
    plug :need_assistant_aff
  end

  pipeline :course_self do
    plug :need_self_aff
  end
  
  pipeline :course_member do
    plug :need_aff
  end

  
  scope "/", SchoolhubWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/phoenix", PageController, :phoenix
    resources "/users", UserController, only: [:new, :create]
    resources "/sessions", SessionController, only: [:new, :create, :delete],
      singleton: true

    pipe_through :session
    resources "/users", UserController, only: [:show]
    resources "/courses", CourseController, only: [:index]

    
    pipe_through :course_member
    resources "/courses", CourseController, only: [:show] do
      get "/chat", ChatController, :index
      get "/chat/:user_id", ChatController, :chat
      resources "/affiliations", AffiliationController, only: [:index, :show]
      resources "/posts", PostController, except: [:edit, :update, :delete] do
	resources "/replies", ReplyController, except: [:edit, :update, :delete]
      end
    end

    pipe_through :course_self
    resources "/courses", CourseController, only: [] do
      resources "/affiliations", AffiliationController, only: [] do
	resources "/grades", GradeController, only: [:show]
      end
    end

    pipe_through :course_assistant
    resources "/courses/assistant", CourseController, except: [:index, :show, :new, :create, :delete] do
      resources "/grades", GradeController, only: [:index]
      resources "/affiliations", AffiliationController, except: [:index, :show, :edit, :update] do
	resources "/grades", GradeController, except: [:show, :index]
      end
      
      resources "/posts", PostController, only: [:edit, :update, :delete] do
	resources "/replies", ReplyController, only: [:edit, :update, :delete]
	put "/pin", PostController, :pin
      end
    end

    pipe_through :course_owner
    resources "/courses/owner", CourseController, only: [:delete] do
      resources "/affiliations", AffiliationController, only: [:edit, :update]
    end
    
  end

  scope "/teacher", SchoolhubWeb do
    pipe_through :browser
    pipe_through :session
    pipe_through :teacher

    resources "/courses", CourseController, only: [:new, :create]
  end

  scope "/admin", SchoolhubWeb do
    pipe_through :browser
    pipe_through :session
    #pipe_through :admin

    resources "/privileges", PrivilegeController, except: [:new, :create, :delete]
    resources "/users", UserController, except: [:new, :create, :show]
    get "/courses", CourseController, :admin_index
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

end
