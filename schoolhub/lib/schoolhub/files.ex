defmodule Schoolhub.Files do
  @moduledoc """
  The Files context.
  """

  import Ecto.Query, warn: false
  alias Schoolhub.Repo

  alias Schoolhub.Files.{File, FileData}

  @doc """
  Returns the list of files.

  ## Examples

      iex> list_files()
      [%File{}, ...]

  """
  def list_files do
    Repo.all(File)
  end

  @doc """
  Gets a single file.

  Raises `Ecto.NoResultsError` if the File does not exist.

  ## Examples

      iex> get_file!(123)
      %File{}

      iex> get_file!(456)
      ** (Ecto.NoResultsError)

  """
  def get_file!(id), do: Repo.get!(File, id)

  @doc """
  Creates a file.

  ## Examples

      iex> create_file(%{field: value})
      {:ok, %File{}}

      iex> create_file(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_file(attrs \\ %{}) do
    %File{}
    |> File.changeset(attrs)
    |> Ecto.Changeset.cast_assoc(:file_data, with: &FileData.changeset/2)
    |> Repo.insert()
  end

  @doc """
  Updates a file.

  ## Examples

      iex> update_file(file, %{field: new_value})
      {:ok, %File{}}

      iex> update_file(file, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_file(%File{} = file, attrs) do
    file
    |> File.changeset(attrs)
    |> Ecto.Changeset.cast_assoc(:file_data, with: &FileData.changeset/2)
    |> Repo.update()
  end

  @doc """
  Deletes a file.

  ## Examples

      iex> delete_file(file)
      {:ok, %File{}}

      iex> delete_file(file)
      {:error, %Ecto.Changeset{}}

  """
  def delete_file(%File{} = file) do
    Repo.delete(file)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file changes.

  ## Examples

      iex> change_file(file)
      %Ecto.Changeset{data: %File{}}

  """
  def change_file(%File{} = file, attrs \\ %{}) do
    File.changeset(file, attrs)
  end


  @doc """
  Returns the list of file_data.

  ## Examples

      iex> list_file_data()
      [%FileData{}, ...]

  """
  def list_file_data do
    Repo.all(FileData)
  end

  @doc """
  Gets a single file_data.

  Raises `Ecto.NoResultsError` if the File data does not exist.

  ## Examples

      iex> get_file_data!(123)
      %FileData{}

      iex> get_file_data!(456)
      ** (Ecto.NoResultsError)

  """
  def get_file_data!(id), do: Repo.get!(FileData, id)

  def get_file_data_by_file!(file_id), do: Repo.get_by!(FileData, file_id: file_id)

  @doc """
  Creates a file_data.

  ## Examples

      iex> create_file_data(%{field: value})
      {:ok, %FileData{}}

      iex> create_file_data(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_file_data(attrs \\ %{}) do
    %FileData{}
    |> FileData.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a file_data.

  ## Examples

      iex> update_file_data(file_data, %{field: new_value})
      {:ok, %FileData{}}

      iex> update_file_data(file_data, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_file_data(%FileData{} = file_data, attrs) do
    file_data
    |> FileData.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a file_data.

  ## Examples

      iex> delete_file_data(file_data)
      {:ok, %FileData{}}

      iex> delete_file_data(file_data)
      {:error, %Ecto.Changeset{}}

  """
  def delete_file_data(%FileData{} = file_data) do
    Repo.delete(file_data)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking file_data changes.

  ## Examples

      iex> change_file_data(file_data)
      %Ecto.Changeset{data: %FileData{}}

  """
  def change_file_data(%FileData{} = file_data, attrs \\ %{}) do
    FileData.changeset(file_data, attrs)
  end
end
