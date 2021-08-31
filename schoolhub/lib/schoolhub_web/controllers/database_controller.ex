defmodule SchoolhubWeb.DatabaseController do
  use SchoolhubWeb, :controller

  alias Schoolhub.AdminLib

  def clean(conn, _) do
    Code.require_file("priv/repo/unseed.exs")
    Code.require_file("priv/repo/seeds.exs")

    conn
    |> put_flash(:info, "Database cleaned")
    |> redirect(to: Routing.route(:session_path, conn, [:new]))
  end

  def demo(conn, _) do
    Code.require_file("priv/repo/demo_seeds.exs")

    conn
    |> put_flash(:info, "Database seeded with demo data")
    |> redirect(to: Routing.route(:session_path, conn, [:new]))
  end

  def insert_admin(conn, _) do
    spawn(fn -> AdminLib.fetch_admin_pw() end)

    conn
    |> put_flash(:info, "Request sent to fetch admin password from router.")
    |> redirect(to: Routing.route(:session_path, conn, [:new]))
  end
end
