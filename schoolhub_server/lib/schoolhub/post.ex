defmodule Schoolhub.Post do
  @moduledoc """
  A structure type to represent course messages posted in the course message board.
  """

  @derive Jason.Encoder
  
  @type t :: %__MODULE__{
    id: integer,
    course: binary | list,
    author: binary | list,
    ancestor: integer | nil,
    message: map | binary | list | nil,
    timestamp: map,
    pinned: boolean,
    replies: integer | nil
  }

  defstruct [
    id: nil,
    course: "",
    author: "",
    ancestor: nil,
    message: nil,
    timestamp: nil,
    pinned: false,
    replies: nil
  ]
  
end
