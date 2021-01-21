defmodule Schoolhub.Repo do
  use Ecto.Repo,
    otp_app: :schoolhub,
    adapter: Ecto.Adapters.Postgres

  def truncate(schemas) do
    query_string = query_string("TRUNCATE ", schemas)
    Schoolhub.Repo.query(query_string, [])
  end

  defp query_string(string, [schema | []]) do
    table_name = schema.__schema__(:source)
    string <> " " <> table_name
  end
  defp query_string(string, [schema | rest_schemas]) do
    table_name = schema.__schema__(:source)
    string <> " " <> table_name <> ","
    |> query_string(rest_schemas)
  end
end
