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

admin_pw = "==SCRAM==,B5MvA8tsxOLvGXYEv7uYF9ZeOPU=,je1u7PlEhL/U/KEQKRwCZkGi5oc=,G5FcyAd91/ViouVZOzwm/w==,4096"

admin = %User{name: "Admin McAdminson",
	      email: "admin@admin.com",
	      credential: %Credential{username: "admin",
				      password: "",
				      pass_details: admin_pw},
	      privilege: %Privilege{level: "admin"}}

Schoolhub.Repo.insert!(admin)

#TODO: extract pw as secret, protect from errors
