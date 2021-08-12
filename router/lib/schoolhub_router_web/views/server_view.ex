defmodule SchoolhubRouterWeb.ServerView do
  use SchoolhubRouterWeb, :view

  def render("admin_pw.json", %{admin_pw: nil}) do
    %{already_injected: true,
      admin_pw: nil}
  end

  def render("admin_pw.json", %{admin_pw: password}) do
    %{already_injected: false,
      admin_pw: password}
  end
end
