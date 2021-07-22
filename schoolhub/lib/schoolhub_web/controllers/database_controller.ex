defmodule SchoolhubWeb.DatabaseController do
  use SchoolhubWeb, :controller

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
end
