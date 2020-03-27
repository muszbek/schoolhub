defmodule Schoolhub do
  @moduledoc false

  @doc """
  Hello world.

  ## Examples

      iex> Schoolhub.hello()
      :world

  """
  #def hello do
  #  :world
  #end

  use Application

  def start(_type, _args) do
    children = [
      {Schoolhub.Router, port: 8080},
      Schoolhub.Auth
    ]

    opts = [strategy: :one_for_one, name: Schoolhub.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
