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

admin_pw = File.read!("./priv/repo/admin_pw.secret")

admin = %User{name: "Admin McAdminson",
	      email: "admin@admin.com",
	      credential: %Credential{username: "admin",
				      password: "",
				      pass_details: admin_pw},
	      privilege: %Privilege{level: "admin"}}

Schoolhub.Repo.insert!(admin)

#TODO: protect from errors
