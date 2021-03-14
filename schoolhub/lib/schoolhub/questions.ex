defmodule Schoolhub.Questions do
  @moduledoc """
  The Questions context.
  """

  import Ecto.Query, warn: false
  alias Schoolhub.Repo

  alias Schoolhub.Questions.{Question, Qreply, Follow}

  @question_limit_default 5

  @doc """
  Returns the list of questions.

  ## Examples

      iex> list_questions()
      [%Question{}, ...]

  """
  def list_course_questions(course_id, question_limit \\ @question_limit_default) do
    Question
    |> where(course_id: ^course_id)
    |> order_by(desc: :pinned)
    |> order_by(desc: :inserted_at)
    |> limit(^question_limit)
    |> Repo.all()
    |> Repo.preload(:qreply)
  end

  def filter_questions(course_id, user_id, filters \\ [], following \\ "all") do
    Question
    |> where(course_id: ^course_id)
    |> filter_following(user_id, following)
    |> order_by(desc: :pinned)
    |> order_by(desc: :inserted_at)
    |> where([q], fragment("? @> ?::varchar[]", q.tags, ^filters))
    |> Repo.all()
    |> Repo.preload(:qreply)
  end

  defp filter_following(query, user_id, following) do
    case following do
      "all" -> query
      "only_following" ->
	followed = followed_questions_subquery(user_id)
	
	query
	|> where([q], q.id in subquery(followed))
    end
  end
  
  defp followed_questions_subquery(user_id) do
    Follow
    |> where(user_id: ^user_id)
    |> select([:question_id])
  end

  @doc """
  Gets a single question.

  Raises `Ecto.NoResultsError` if the Question does not exist.

  ## Examples

      iex> get_question!(123)
      %Question{}

      iex> get_question!(456)
      ** (Ecto.NoResultsError)

  """
  def get_question!(id) do
    Question
    |> Repo.get!(id)
    |> Repo.preload(:qreply)
  end

  @doc """
  Creates a question.

  ## Examples

      iex> create_question(%{field: value})
      {:ok, %Question{}}

      iex> create_question(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_question(attrs \\ %{}) do
    %Question{}
    |> Question.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a question.

  ## Examples

      iex> update_question(question, %{field: new_value})
      {:ok, %Question{}}

      iex> update_question(question, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_question(%Question{} = question, attrs) do
    question
    |> Question.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a question.

  ## Examples

      iex> delete_question(question)
      {:ok, %Question{}}

      iex> delete_question(question)
      {:error, %Ecto.Changeset{}}

  """
  def delete_question(%Question{} = question) do
    Repo.delete(question)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking question changes.

  ## Examples

      iex> change_question(question)
      %Ecto.Changeset{data: %Question{}}

  """
  def change_question(%Question{} = question, attrs \\ %{}) do
    Question.changeset(question, attrs)
  end

  alias Schoolhub.Questions.Qreply

  @doc """
  Returns the list of question_replies.

  ## Examples

      iex> list_question_replies()
      [%Qreply{}, ...]

  """
  def list_question_replies do
    Repo.all(Qreply)
  end

  @doc """
  Gets a single qreply.

  Raises `Ecto.NoResultsError` if the Qreply does not exist.

  ## Examples

      iex> get_qreply!(123)
      %Qreply{}

      iex> get_qreply!(456)
      ** (Ecto.NoResultsError)

  """
  def get_qreply!(id), do: Repo.get!(Qreply, id)

  @doc """
  Creates a qreply.

  ## Examples

      iex> create_qreply(%{field: value})
      {:ok, %Qreply{}}

      iex> create_qreply(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_qreply(attrs \\ %{}) do
    %Qreply{}
    |> Qreply.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a qreply.

  ## Examples

      iex> update_qreply(qreply, %{field: new_value})
      {:ok, %Qreply{}}

      iex> update_qreply(qreply, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_qreply(%Qreply{} = qreply, attrs) do
    qreply
    |> Qreply.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a qreply.

  ## Examples

      iex> delete_qreply(qreply)
      {:ok, %Qreply{}}

      iex> delete_qreply(qreply)
      {:error, %Ecto.Changeset{}}

  """
  def delete_qreply(%Qreply{} = qreply) do
    Repo.delete(qreply)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking qreply changes.

  ## Examples

      iex> change_qreply(qreply)
      %Ecto.Changeset{data: %Qreply{}}

  """
  def change_qreply(%Qreply{} = qreply, attrs \\ %{}) do
    Qreply.changeset(qreply, attrs)
  end


  @doc """
  Gets a single follow.

  Raises `Ecto.NoResultsError` if the Follow does not exist.

  ## Examples

      iex> get_follow!(123)
      %Follow{}

      iex> get_follow!(456)
      ** (Ecto.NoResultsError)

  """
  def get_follow!(id), do: Repo.get!(Follow, id)

  def get_follow(question_id, user_id) do
    Follow
    |> where(question_id: ^question_id)
    |> where(user_id: ^user_id)
    |> Repo.all()
  end

  def get_follows_number(question_id) do
    Follow
    |> where(question_id: ^question_id)
    |> Repo.aggregate(:count)
  end
  
  @doc """
  Creates a follow.

  ## Examples

      iex> create_follow(%{field: value})
      {:ok, %Follow{}}

      iex> create_follow(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_follow(attrs \\ %{}) do
    %Follow{}
    |> Follow.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Deletes a follow.

  ## Examples

      iex> delete_follow(follow)
      {:ok, %Follow{}}

      iex> delete_follow(follow)
      {:error, %Ecto.Changeset{}}

  """
  def delete_follow(%Follow{} = follow) do
    Repo.delete(follow)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking follow changes.

  ## Examples

      iex> change_follow(follow)
      %Ecto.Changeset{data: %Follow{}}

  """
  def change_follow(%Follow{} = follow, attrs \\ %{}) do
    Follow.changeset(follow, attrs)
  end

end
