defmodule Schoolhub.Courses do
  @moduledoc """
  The Courses context.
  """
  
  import Ecto.Query, warn: false
  alias Schoolhub.Repo

  alias Schoolhub.Courses.{Course, Affiliation}
  alias Schoolhub.Grades.Grade
  alias Schoolhub.Accounts

  @default_grade %{}
  
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
    |> Repo.preload(:post)
  end

  def list_affiliated_courses(user_id) do
    affiliations = Affiliation
    |> where(user_id: ^user_id)
    |> Repo.all()

    for aff <- affiliations, do: get_course!(aff.course_id)
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
    |> Repo.preload(:post)
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


  @doc """
  Returns the list of course_affiliations.

  ## Examples

      iex> list_course_affiliations()
      [%Affiliation{}, ...]

  """
  def list_course_affiliations(course_id) do
    Affiliation
    |> where(course_id: ^course_id)
    |> order_by(desc: :affiliation)
    |> Repo.all()
    |> Repo.preload(:grade)
  end

  def get_username!(user_id, course_id) do
    %Affiliation{affiliation: aff} = get_affiliation_by_user!(course_id, user_id)
    user = Accounts.get_user!(user_id)
    
    case aff do
      "student" ->
	user.name
      other ->
	user.name <> " (" <> other <> ")"
    end
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
  def get_affiliation!(id) do
    Affiliation
    |> Repo.get!(id)
    |> Repo.preload(:grade)
  end

  def get_owner!(course_id) do
    Affiliation
    |> Repo.get_by!([course_id: course_id, affiliation: "owner"])
    |> Repo.preload(:grade)
  end

  def get_affiliation_by_user!(course_id, user_id) do
    Affiliation
    |> Repo.get_by!([course_id: course_id, user_id: user_id])
    |> Repo.preload(:grade)
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
    attrs_with_grade = attrs
    |> Map.put("grade", %{grades: @default_grade})
    |> Morphix.atomorphify!()
    
    %Affiliation{}
    |> Affiliation.changeset(attrs_with_grade)
    |> Ecto.Changeset.cast_assoc(:grade, with: &Grade.changeset/2)
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
