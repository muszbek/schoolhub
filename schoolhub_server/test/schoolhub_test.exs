defmodule SchoolhubTest do
  use ExUnit.Case
  doctest Schoolhub

  test "greets the world" do
    assert Schoolhub.hello() == :world
  end
end
