# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Schoolhub.Repo.insert!(%Schoolhub.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias Schoolhub.Accounts.{User, Credential}
alias Schoolhub.Privileges.Privilege
alias Schoolhub.Accounts.ScramLib, as: CredLib

admin_pw =
  File.read!("./priv/repo/admin_pw.secret")
  |> CredLib.encode_password()

admin = %User{name: "Sir Adminson His Highness",
	      email: "high_admin@admin.com",
	      credential: %Credential{username: "high_admin",
				      password: "",
				      pass_details: admin_pw},
	      privilege: %Privilege{level: "admin"}}

try do
  Schoolhub.Repo.insert!(admin)
  IO.puts("High admin user inserted")
rescue
  Ecto.ConstraintError -> IO.puts("High admin user already inserted...")
end
