defmodule Schoolhub.AdminLib do
  @moduledoc """
  Library module for startup task that creates the admin user.
  """

  alias Schoolhub.Accounts.{User, Credential}
  alias Schoolhub.Privileges.Privilege
  alias Schoolhub.Accounts.ScramLib, as: CredLib

  require Logger

  def fetch_admin_pw() do
    pod_name = Application.get_env(:schoolhub, __MODULE__)[:self_pod_name]
    host = Application.get_env(:schoolhub, __MODULE__)[:router_host]
    port = Application.get_env(:schoolhub, __MODULE__)[:router_port] |> to_string()
    url = "https://" <> host <> ":" <> port <> "/router/admin_pw/" <> pod_name

    ssl_opts = Application.get_env(:schoolhub, __MODULE__)[:ssl_opts]
    options = [ssl: ssl_opts]

    response = HTTPoison.get!(url, [], options)
    response_body = Jason.decode!(response.body)

    handle_response(response_body)
  end

  defp handle_response(%{"already_injected" => true}) do
    Logger.info("Admin user already inserted...")
  end
  defp handle_response(%{"already_injected" => false, "admin_pw" => password}) do
    create_admin(password)
  end

  defp create_admin(password) do
    encoded_pw = CredLib.encode_password(password)

    admin = %User{name: "Admin McAdminson",
		  email: "admin@admin.com",
		  credential: %Credential{username: "admin",
					  password: "",
					  pass_details: encoded_pw},
		  privilege: %Privilege{level: "admin"}}

    try do
      Schoolhub.Repo.insert!(admin)
      Logger.info("Admin user inserted")
    rescue
      Ecto.ConstraintError -> Logger.warn("Admin user already exists...")
    end
  end

end
