defmodule Schoolhub.MixProject do
  use Mix.Project

  def project do
    [
      app: :schoolhub,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Schoolhub, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:romeo, git: "https://github.com/scrogson/romeo"},
      {:plug_cowboy, "~> 2.0"},
      {:jason, "~> 1.0"},
      {:ecto_ltree, "~> 0.2.0"},
      {:scramerl, git: "https://github.com/erdemaksu/scramerl"},
      {:distillery, "~> 2.0"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
