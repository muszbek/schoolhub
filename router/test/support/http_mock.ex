defmodule SchoolhubRouterWeb.HttpMock do
  @moduledoc """
  Test support module to mock the behaviour of HTTPoison.
  """

  def get(_url, _, _options) do
    {:ok, %{status_code: 200}}
    # {:error, %HTTPoison.Error{}}
  end
end
