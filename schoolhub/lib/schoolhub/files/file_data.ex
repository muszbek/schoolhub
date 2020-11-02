defmodule Schoolhub.Files.FileData do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Files.File
  
  schema "file_data" do
    field :data, :binary
    belongs_to :file, File

    timestamps()
  end

  @doc false
  def changeset(file_data, attrs) do
    file_data
    |> cast(attrs, [:data, :file_id])
    |> validate_required([:data])
    |> foreign_key_constraint(:file_id)
  end
end
