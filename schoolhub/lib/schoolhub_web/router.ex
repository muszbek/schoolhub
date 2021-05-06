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

  pipeline :course_enabled do
    plug :need_course_enabled
  end
  
  pipeline :course_member do
    plug :need_aff
  end

  
  scope "/", SchoolhubWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/phoenix", PageController, :phoenix
    resources "/users", UserController, only: [:new, :create]
    get "/users/change_pw/:token", UserController, :change_pw
    put "/users/change_pw/:token", UserController, :update_pw
    get "/users/register/:token", UserController, :confirm
    resources "/sessions", SessionController, only: [:new, :create, :delete],
      singleton: true
    get "/sessions/forgot_pw", SessionController, :forgot_pw
    post "/sessions/forgot_pw", SessionController, :send_email

    pipe_through :session
    post "/token", SessionController, :renew_token
    delete "/users", UserController, :self_delete
    get "/users", UserController, :show_self
    resources "/courses", CourseController, only: [:index]
    resources "/course_token", TokenController, only: [:new, :create]

    
    pipe_through :course_member
    resources "/courses", CourseController, only: [:show] do
      resources "/affiliations", AffiliationController, only: [:index, :show] do
	get "/grades", GradeController, :show_self
      end
      
      resources "/posts/public", PostController, only: [:index, :show] do
	resources "/replies", ReplyController, only: [:index, :show]
      end
      get "/questions/filters/:filters/:only_following", QuestionController, :filter
      resources "/questions/public", QuestionController, only: [:index, :show] do
	resources "/question_replies", QreplyController, only: [:index, :show]
      end
      
      resources "/files", FileController, only: [:index, :show]
      get "/files/download/:id", FileController, :download
      
    end

    pipe_through :course_enabled
    resources "/courses", CourseController, only: [] do
      get "/chat", ChatController, :index
      get "/chat/:user_id", ChatController, :chat

      resources "/posts", PostController, only: [] do
	resources "/replies", ReplyController, only: [:new, :create]
      end
      resources "/questions", QuestionController, only: [:new, :create] do
	resources "/question_replies", QreplyController, only: [:new, :create]
	post "/follow", FollowController, :follow
	delete "/unfollow", FollowController, :unfollow
      end
    end

    pipe_through :course_self
    resources "/courses", CourseController, only: [] do
      resources "/posts", PostController, only: [] do
	resources "/replies", ReplyController, except: [:index, :show, :new, :create]
      end
      resources "/questions", QuestionController, except: [:index, :show, :new, :create] do
	resources "/question_replies", QreplyController, except: [:index, :show, :new, :create]
      end
    end

    pipe_through :course_assistant
    resources "/courses/assistant", CourseController, except: [:index, :show, :new, :create, :delete] do
      get "/token", CourseController, :new_token
      resources "/grades", GradeController, only: [:index]
      resources "/affiliations", AffiliationController, except: [:index, :show, :edit, :update] do
	resources "/grades", GradeController, except: [:index]
      end
      
      resources "/posts", PostController, except: [:index, :show] do
	put "/pin", PostController, :pin
      end
      resources "/questions", QuestionController, only: [] do
	put "/pin", QuestionController, :pin
      end

      resources "/files", FileController, except: [:index, :show]
    end

    pipe_through :course_owner
    resources "/courses/owner", CourseController, only: [:delete] do
      resources "/affiliations", AffiliationController, only: [:edit, :update]
    end
    
  end

  scope "/activate", SchoolhubWeb do
    pipe_through :browser
    pipe_through :session
    pipe_through :course_owner

    resources "/courses", CourseController, only: [] do
      put "/activate", CourseController, :activate
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
    pipe_through :admin

    resources "/privileges", PrivilegeController, except: [:new, :create, :delete]
    resources "/users", UserController, except: [:new, :create]
    get "/courses", CourseController, :admin_index
  end

  scope "/auth", SchoolhubWeb do
    pipe_through :api

    post "/", SessionController, :authenticate
  end

  scope "/database", SchoolhubWeb do
    pipe_through :browser
    pipe_through :session
    pipe_through :admin

    get "/clean", DatabaseController, :clean
    get "/demo", DatabaseController, :demo
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
