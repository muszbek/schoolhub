defmodule SchoolhubWeb.HttpMock do
  @moduledoc """
  Test support module to mock the behaviour of HTTPoison.
  """

  def get!(_url, _, _options) do
    %{status_code: 200, body: "{\"already_injected\": true}"}
  end
end
