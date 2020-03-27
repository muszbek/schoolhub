defmodule SchoolhubClientTest do
  use ExUnit.Case
  doctest SchoolhubClient

  test "greets the world" do
    assert SchoolhubClient.hello() == :world
  end
end
