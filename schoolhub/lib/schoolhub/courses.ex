defmodule Schoolhub.Courses do
  @moduledoc """
  The Courses context.
  """
  
  import Ecto.Query, warn: false
  alias Schoolhub.Repo

  alias Schoolhub.Courses.Course

  @doc """
  Returns the list of courses.

  ## Examples

      iex> list_courses()
      [%Course{}, ...]

  """
  def list_courses do
    Course
    |> Repo.all()
    |> Repo.preload(:affiliation)
  end

  @doc """
  Gets a single course.

  Raises `Ecto.NoResultsError` if the Course does not exist.

  ## Examples

      iex> get_course!(123)
      %Course{}

      iex> get_course!(456)
      ** (Ecto.NoResultsError)

  """
  def get_course!(id) do
    Course
    |> Repo.get!(id)
    |> Repo.preload(:affiliation)
  end

  @doc """
  Creates a course.

  ## Examples

      iex> create_course(%{field: value})
      {:ok, %Course{}}

      iex> create_course(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_course(attrs \\ %{}) do
    %Course{}
    |> Course.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a course.

  ## Examples

      iex> update_course(course, %{field: new_value})
      {:ok, %Course{}}

      iex> update_course(course, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_course(%Course{} = course, attrs) do
    course
    |> Course.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a course.

  ## Examples

      iex> delete_course(course)
      {:ok, %Course{}}

      iex> delete_course(course)
      {:error, %Ecto.Changeset{}}

  """
  def delete_course(%Course{} = course) do
    Repo.delete(course)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking course changes.

  ## Examples

      iex> change_course(course)
      %Ecto.Changeset{data: %Course{}}

  """
  def change_course(%Course{} = course, attrs \\ %{}) do
    Course.changeset(course, attrs)
  end

  alias Schoolhub.Courses.Affiliation

  @doc """
  Returns the list of course_affiliations.

  ## Examples

      iex> list_course_affiliations()
      [%Affiliation{}, ...]

  """
  def list_course_affiliations do
    Repo.all(Affiliation)
  end

  @doc """
  Gets a single affiliation.

  Raises `Ecto.NoResultsError` if the Affiliation does not exist.

  ## Examples

      iex> get_affiliation!(123)
      %Affiliation{}

      iex> get_affiliation!(456)
      ** (Ecto.NoResultsError)

  """
  def get_affiliation!(id), do: Repo.get!(Affiliation, id)

  def get_owner!(course_id) do
    Repo.get_by!(Affiliation, [course_id: course_id, affiliation: "owner"])
  end

  @doc """
  Creates a affiliation.

  ## Examples

      iex> create_affiliation(%{field: value})
      {:ok, %Affiliation{}}

      iex> create_affiliation(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_affiliation(attrs \\ %{}) do
    %Affiliation{}
    |> Affiliation.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a affiliation.

  ## Examples

      iex> update_affiliation(affiliation, %{field: new_value})
      {:ok, %Affiliation{}}

      iex> update_affiliation(affiliation, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_affiliation(%Affiliation{} = affiliation, attrs) do
    affiliation
    |> Affiliation.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a affiliation.

  ## Examples

      iex> delete_affiliation(affiliation)
      {:ok, %Affiliation{}}

      iex> delete_affiliation(affiliation)
      {:error, %Ecto.Changeset{}}

  """
  def delete_affiliation(%Affiliation{} = affiliation) do
    Repo.delete(affiliation)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking affiliation changes.

  ## Examples

      iex> change_affiliation(affiliation)
      %Ecto.Changeset{data: %Affiliation{}}

  """
  def change_affiliation(%Affiliation{} = affiliation, attrs \\ %{}) do
    Affiliation.changeset(affiliation, attrs)
  end
end
