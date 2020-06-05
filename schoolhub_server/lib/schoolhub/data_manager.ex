defmodule Schoolhub.DataManager do
  @moduledoc """
  API for accessing the Postgres database.
  Concerns generic data, course contents are handled by Schoolhub.ContentManager.
  """
  require Logger

  use GenServer

  defstruct(
    pgsql_conn: :nil
  )

  ### API functions ###

  @doc false
  def start_link([name: name]) do
    GenServer.start_link(name, :ok, name: __MODULE__)
  end

  @doc false
  def start_link([]) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @doc """
  Fetching the scram data entry associated with username in the database.
  The scram data entry is a composition of different hashed values, decoded in Schoolhub.Auth.
  """
  def get_scram_pw(username) do
    GenServer.call(__MODULE__, {:get_pw, username})
  end

  @doc """
  Returns true if an entry with 'username' already exists within the database,
  otherwise false.
  """
  def check_user_exist(username) do
    _does_exist = GenServer.call(__MODULE__, {:check_user_exist, username})
  end

  @doc """
  Adds the same database user entry as mongooseim.
  The caller should construct the scram 'pass_details' (normally Schoolhub.RegServer)
  """
  def add_scram_user(username, pass_details) do
    GenServer.call(__MODULE__, {:add_scram_user, username, pass_details})
  end

  @doc """
  Removes the database entry associated with 'username'.
  """
  def remove_scram_user(username) do
    GenServer.call(__MODULE__, {:remove_scram_user, username})
  end

  @doc """
  Removes the database entry and every reference associated with 'username'.
  """
  def purge_user(username) do
    GenServer.call(__MODULE__, {:purge_user, username})
  end

  @doc """
  Fetches XMPP message archive between two users.
  """
  def get_archive(self, partner, limit) do
    GenServer.call(__MODULE__, {:get_archive, self, partner, limit})
  end

  @doc """
  Adds user to user_privileges table when
  creating new user through mongooseim.
  Default privilege is 'student', an admin has to change it.
  """
  def add_user_privilege(username) do
    GenServer.call(__MODULE__, {:add_privilege, username})
  end

  @doc """
  Interrogates the privilege level of the user.
  'student' -> normal user with restricted privileges, default
  'teacher' -> can create courses
  'admin' -> can create courses and changes privileges of others
  """
  def get_user_privilege(username) do
    GenServer.call(__MODULE__, {:get_privilege, username})
  end

  @doc """
  Returns a list of all users with their privileges.
  """
  def get_all_privilege() do
    GenServer.call(__MODULE__, :get_all_privilege)
  end

  @doc """
  Sets the privilege level of the user.
  The RegServer controls whether the user has right to do this.
  """
  def set_user_privilege(username, privilege) do
    GenServer.call(__MODULE__, {:set_privilege, username, privilege})
  end

  @doc """
  Creates a course entry with the given name and adding the owner user with owner affiliation.
  """
  def create_course(course_name, owner) do
    GenServer.call(__MODULE__, {:create_course, course_name, owner})
  end

  @doc """
  Removes course entry from courses table and all related affiliations.
  """
  def remove_course(name) do
    GenServer.call(__MODULE__, {:remove_course, name})
  end
  
  @doc """
  Interrogates affiliation of user to a given course, from the course_affiliations table.
  """
  def get_affiliation(user, course_name) do
    GenServer.call(__MODULE__, {:get_affiliation, user, course_name})
  end

  @doc """
  Interrogates affiliation of all users to a given course, from the course_affiliations table.
  """
  def get_all_affiliation(course_name) do
    GenServer.call(__MODULE__, {:get_all_affiliation, course_name})
  end

  @doc """
  Updates the affiliation of the user to the specified course,
  if it already has been invited.
  """
  def set_affiliation(user, course_name, aff) do
    GenServer.call(__MODULE__, {:set_affiliation, user, course_name, aff})
  end

  @doc """
  Adds a student affiliation to the user with the specified course
  (if course exists).
  """
  def invite_student(user, course_name) do
    GenServer.call(__MODULE__, {:invite_student, user, course_name})
  end

  @doc """
  Removes the affiliation of the user with specified course.
  """
  def remove_student(user, course_name) do
    GenServer.call(__MODULE__, {:remove_student, user, course_name})
  end

  @doc """
  Interrogates the database for the description field of the course.
  """
  def get_course_desc(course_name) do
    GenServer.call(__MODULE__, {:get_desc, course_name})
  end
  
  @doc """
  Modifies the description field of the course.
  """
  def set_course_desc(course_name, desc) do
    GenServer.call(__MODULE__, {:set_desc, course_name, desc})
  end
    

  ### Server callbacks ###
  
  @impl true
  def init(:ok) do
    args = Application.get_env(:schoolhub, :postgres_address)
    
    {:ok, pid} = Postgrex.start_link(args)
    {:ok, %__MODULE__{pgsql_conn: pid}}
  end

  @impl true
  def handle_call({:get_pw, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT pass_details FROM users WHERE username LIKE $1"
    {:ok, data} = Postgrex.query(conn, query_text, [string(username)])

    case data.rows do
      [] ->
	Logger.debug("#{inspect(username)} is not present in database")
        {:reply, :nil, state}
      [[result]] ->
	Logger.debug("Fetching scram data from database for username #{inspect(username)}: "
	  <> "#{inspect(result)}")
        {:reply, result, state}
    end
  end

  @impl true
  def handle_call({:check_user_exist, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT username FROM users WHERE username LIKE $1"
    {:ok, data} = Postgrex.query(conn, query_text, [string(username)])

    case data.rows do
      [] ->
        {:reply, false, state}
      [[^username]] ->
        {:reply, true, state}
    end
  end

  @impl true
  def handle_call({:add_scram_user, username, pass_details}, _from, state = %{pgsql_conn: conn}) do
    query_text = "INSERT INTO users (username, password, pass_details) VALUES ($1, '', $2);"
    result = Postgrex.query(conn, query_text, [string(username), string(pass_details)])

    case result do
      {:ok, %{columns: nil, command: :insert, messages: [], num_rows: 1, rows: nil}} ->
	{:reply, :ok, state}
      {:error, %Postgrex.Error{postgres: %{code: :unique_violation,
					   constraint: "users_pkey",
					   severity: "ERROR",
					   table: "users"}}} ->
	{:reply, :user_exists, state}
    end
  end

  @impl true
  def handle_call({:remove_scram_user, username}, _from, state = %{pgsql_conn: conn}) do
    result = remove_user(username, conn)
    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:purge_user, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "DELETE FROM mam_message WHERE remote_bare_jid LIKE $1;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_text, [string(username)])
    result = remove_user(username, conn)
    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:get_archive, self, partner, limit}, _from, state = %{pgsql_conn: conn}) do
    limit_query = case limit do
		    :all -> ""
		    number when is_integer(number) and number > 0 ->
		      "LIMIT " <> string(number)
		  end
    id_query = "SELECT id FROM mam_server_user WHERE user_name LIKE $1"
    query_text = "SELECT direction, search_body FROM mam_message WHERE user_id = (" <>
      id_query <> ") AND remote_bare_jid LIKE $2 " <> limit_query <> ";"
    
    {:ok, data} = Postgrex.query(conn, query_text, [string(self), string(partner)])
    {:reply, data.rows, state}
  end

  @impl true
  def handle_call({:add_privilege, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "INSERT INTO user_privileges (username, permission) VALUES ($1, 'student');"
    result = Postgrex.query(conn, query_text, [string(username)])
    {:ok, %{columns: nil, command: :insert, messages: [], num_rows: 1, rows: nil}} = result
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:get_privilege, username}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT permission FROM user_privileges WHERE username LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(username)])
    
    permission = case result do
		   {:ok, %{columns: ["permission"], num_rows: 1, rows: [[perm]]}} ->
		     perm
		   {:ok, %{columns: ["permission"], num_rows: 0, rows: []}} ->
		     :nil
		 end
    
    {:reply, permission, state}
  end

  @impl true
  def handle_call(:get_all_privilege, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT username, permission FROM user_privileges ORDER BY permission DESC;"
    result = Postgrex.query(conn, query_text, [])
    {:ok, %{columns: ["username", "permission"], command: :select, rows: privileges}} = result
    {:reply, privileges, state}
  end

  @impl true
  def handle_call({:set_privilege, username, privilege}, _from, state = %{pgsql_conn: conn}) do
    query_text = "UPDATE user_privileges SET permission = $2 WHERE username LIKE $1;"
    query_result = Postgrex.query(conn, query_text, [string(username), string(privilege)])

    result = case query_result do
	       {:ok, %{command: :update, num_rows: 1, rows: nil}} -> :ok
	       {:ok, %{command: :update, num_rows: 0, rows: nil}} -> {:error, :user_not_exist}
	     end
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:create_course, course_name, owner}, _from, state = %{pgsql_conn: conn}) do
    query_course = "INSERT INTO courses (name, creator, active) VALUES ($1, $2, true);"
    course_result = Postgrex.query(conn, query_course, [string(course_name), string(owner)])

    result = case course_result do
	       {:ok, %{command: :insert, num_rows: 1, rows: nil}} ->
		 subquery_id = "SELECT id FROM courses WHERE name LIKE $1"
		 query_affiliation = "INSERT INTO course_affiliations " <>
		   "(username, course, affiliation) VALUES ($2, (" <> subquery_id <>
		   "), 'owner');"
		 
		 {:ok, %{command: :insert, num_rows: 1, rows: nil}} =
		   Postgrex.query(conn, query_affiliation, [string(course_name), string(owner)])
		 :ok
		 
               {:error, %{postgres: %{code: :unique_violation,
				      constraint: "courses_name_key",
				      severity: "ERROR",
				      table: "courses"}}} ->
		 {:error, :name_already_used}
	     end
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_course, name}, _from, state = %{pgsql_conn: conn}) do
    subquery_id = "SELECT id FROM courses WHERE name LIKE $1"
    query_affiliation = "DELETE FROM course_affiliations WHERE course = (" <>
      subquery_id <> ");"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_affiliation, [string(name)])

    query_course = "DELETE FROM courses WHERE name LIKE $1;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_course, [string(name)])
    
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:get_affiliation, user, course_name}, _from,
	state = %{pgsql_conn: conn}) do

    aff_tuple = {conn, course_name}
      |> get_course_id()
      |> do_get_affiliation(user)

    result = case aff_tuple do
	       err = {:error, _reason} -> err
	       {_conn, _id, aff} -> aff
	     end
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_all_affiliation, course_name}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_get_all_affiliation()
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:set_affiliation, user, course_name, aff}, _from,
	state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_get_affiliation(user)
      |> do_set_affiliation(user, aff)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:invite_student, user, course_name}, _from, state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> check_user_exist(user)
      |> get_course_id()
      |> do_get_affiliation(user)
      |> do_invite_student(conn, course_name, user)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:remove_student, user, course_name}, _from, state = %{pgsql_conn: conn}) do

    result = {conn, course_name}
      |> get_course_id()
      |> do_get_affiliation(user)
      |> do_remove_student(user)
    
    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_desc, course_name}, _from, state = %{pgsql_conn: conn}) do
    query_text = "SELECT description FROM courses WHERE name LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(course_name)])
    {:ok, %{columns: ["description"], command: :select, num_rows: 1, rows: [[json]]}} = result
    {:reply, json, state}
  end

  @impl true
  def handle_call({:set_desc, course_name, desc}, _from, state = %{pgsql_conn: conn}) do
    query_text = "UPDATE courses SET description = $2 WHERE name LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(course_name), desc])
    {:ok, %{command: :update, num_rows: 1, rows: nil}} = result
    {:reply, :ok, state}
  end

  
  ### Utility functions ###
  
  defp string(text), do: text |> to_string()

  defp remove_user(username, conn) do
    sub_query_mam_id = "SELECT id FROM mam_server_user WHERE user_name LIKE $1"
    query_delete_mam = "DELETE FROM mam_message WHERE user_id = (" <> sub_query_mam_id <> ") ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_delete_mam, [string(username)])

    query_delete_mam_user = "DELETE FROM mam_server_user WHERE user_name LIKE $1 ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_delete_mam_user,
      [string(username)])

    query_delete_affs = "DELETE FROM course_affiliations WHERE username LIKE $1 ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_delete_affs, [string(username)])

    :ok = remove_privilege(username, conn)
    
    query_text = "DELETE FROM users WHERE username LIKE $1;"
    result = Postgrex.query(conn, query_text, [string(username)])

    case result do
      {:ok, %{columns: nil, command: :delete, messages: [], num_rows: 1, rows: nil}} ->
	:user_removed
      {:ok, %{columns: nil, command: :delete, messages: [], num_rows: 0, rows: nil}} ->
	:user_not_existed
    end
  end

  defp remove_privilege(username, conn) do
    query_text = "DELETE FROM user_privileges WHERE username LIKE $1 ;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_text, [string(username)])
    :ok
  end

  defp get_course_id({conn, course_name}) do
    query_id = "SELECT id FROM courses WHERE name LIKE $1;"
    id_result = Postgrex.query(conn, query_id, [string(course_name)])

    case id_result do
      {:ok, %{columns: ["id"], command: :select, num_rows: 1, rows: [[id]]}} ->
	{conn, id}
      
      {:ok, %{columns: ["id"], command: :select, num_rows: 0, rows: []}} ->
	{:error, :course_not_exist}
    end
  end

  defp do_get_affiliation({:error, reason}, _), do: {:error, reason}
  defp do_get_affiliation({conn, id}, user) do
    query_affiliation = "SELECT affiliation FROM course_affiliations WHERE " <>
      "course = $1 AND username LIKE $2;"
    affiliation_result = Postgrex.query(conn, query_affiliation, [id, string(user)])

    case affiliation_result do
      {:ok, %{columns: ["affiliation"], command: :select, num_rows: 1, rows: [[aff]]}} ->
	{conn, id, aff}
      {:ok, %{columns: ["affiliation"], command: :select, num_rows: 0, rows: []}} ->
	{:error, :no_affiliation}
    end
  end

  defp do_get_all_affiliation({:error, reason}), do: {:error, reason}
  defp do_get_all_affiliation({conn, id}) do
    query_text = "SELECT username, affiliation FROM course_affiliations " <>
      "WHERE course = $1 ORDER BY affiliation DESC;"
    affs_result = Postgrex.query(conn, query_text, [id])
		  
    {:ok, %{columns: ["username", "affiliation"], command: :select, rows: affs}} = affs_result
    affs
  end

  defp do_set_affiliation({:error, :no_affiliation}, _, _), do: {:error, :user_not_affiliated}
  defp do_set_affiliation({:error, reason}, _, _), do: {:error, reason}
  defp do_set_affiliation({conn, id, _old_aff}, user, new_aff) do
    query_text = "UPDATE course_affiliations SET affiliation = $3 " <>
      "WHERE username LIKE $1 AND course = $2;"
    {:ok, %{command: :update, num_rows: 1, rows: nil}} =
      Postgrex.query(conn, query_text, [string(user), id, string(new_aff)])

    :ok
  end

  defp check_user_exist({conn, course_name}, user) do
    query_username = "SELECT username FROM users WHERE username LIKE $1;"
    username_result = Postgrex.query(conn, query_username, [string(user)])

    case username_result do
      {:ok, %{columns: ["username"], num_rows: 1, rows: [[^user]]}} ->
	{conn, course_name}
      
      {:ok, %{columns: ["username"], num_rows: 0, rows: []}} ->
	{:error, :user_not_exist}
    end
  end

  defp do_invite_student({:error, :no_affiliation}, conn, course_name, user) do
    subquery_id = "SELECT id FROM courses WHERE name LIKE $1"
	
    query_affiliation = "INSERT INTO course_affiliations " <>
      "(username, course, affiliation) VALUES ($2, (" <> subquery_id <> "), 'student');"
    
    {:ok, %{command: :insert, num_rows: 1, rows: nil}} =
      Postgrex.query(conn, query_affiliation, [string(course_name), string(user)])
	
    :ok
  end
  defp do_invite_student({:error, reason}, _, _, _), do: {:error, reason}
  defp do_invite_student({_conn, _id, _aff}, _, _, _), do: {:ok, :already_invited}

  defp do_remove_student({:error, :no_affiliation}, _), do: {:ok, :already_removed}
  defp do_remove_student({:error, reason}, _), do: {:error, reason}
  defp do_remove_student({conn, id, _aff}, user) do
    query_text = "DELETE FROM course_affiliations WHERE course=$1 AND username=$2;"
    {:ok, %{command: :delete}} = Postgrex.query(conn, query_text, [id, string(user)])
    :ok
  end
end
