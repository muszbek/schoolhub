defmodule SchoolhubWeb.AffiliationControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Courses

  @create_attrs %{affiliation: "some affiliation"}
  @update_attrs %{affiliation: "some updated affiliation"}
  @invalid_attrs %{affiliation: nil}

  def fixture(:affiliation) do
    {:ok, affiliation} = Courses.create_affiliation(@create_attrs)
    affiliation
  end

  describe "index" do
    test "lists all course_affiliations", %{conn: conn} do
      conn = get(conn, Routes.affiliation_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Course affiliations"
    end
  end

  describe "new affiliation" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.affiliation_path(conn, :new))
      assert html_response(conn, 200) =~ "New Affiliation"
    end
  end

  describe "create affiliation" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.affiliation_path(conn, :create), affiliation: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.affiliation_path(conn, :show, id)

      conn = get(conn, Routes.affiliation_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Affiliation"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.affiliation_path(conn, :create), affiliation: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Affiliation"
    end
  end

  describe "edit affiliation" do
    setup [:create_affiliation]

    test "renders form for editing chosen affiliation", %{conn: conn, affiliation: affiliation} do
      conn = get(conn, Routes.affiliation_path(conn, :edit, affiliation))
      assert html_response(conn, 200) =~ "Edit Affiliation"
    end
  end

  describe "update affiliation" do
    setup [:create_affiliation]

    test "redirects when data is valid", %{conn: conn, affiliation: affiliation} do
      conn = put(conn, Routes.affiliation_path(conn, :update, affiliation), affiliation: @update_attrs)
      assert redirected_to(conn) == Routes.affiliation_path(conn, :show, affiliation)

      conn = get(conn, Routes.affiliation_path(conn, :show, affiliation))
      assert html_response(conn, 200) =~ "some updated affiliation"
    end

    test "renders errors when data is invalid", %{conn: conn, affiliation: affiliation} do
      conn = put(conn, Routes.affiliation_path(conn, :update, affiliation), affiliation: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Affiliation"
    end
  end

  describe "delete affiliation" do
    setup [:create_affiliation]

    test "deletes chosen affiliation", %{conn: conn, affiliation: affiliation} do
      conn = delete(conn, Routes.affiliation_path(conn, :delete, affiliation))
      assert redirected_to(conn) == Routes.affiliation_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.affiliation_path(conn, :show, affiliation))
      end
    end
  end

  defp create_affiliation(_) do
    affiliation = fixture(:affiliation)
    %{affiliation: affiliation}
  end
end
