# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     SchoolhubRouter.Repo.insert!(%SchoolhubRouter.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias SchoolhubRouter.Instances.Server

demo_server = %Server{name: "demo",
		      address: "schoolhub-instance-0.schoolhub.default.svc.cluster.local"}

try do
  SchoolhubRouter.Repo.insert!(demo_server)
rescue
  Ecto.ConstraintError -> IO.puts("Demo server already inserted...")
end
