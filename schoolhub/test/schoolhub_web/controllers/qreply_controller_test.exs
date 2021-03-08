defmodule SchoolhubWeb.QreplyControllerTest do
  use SchoolhubWeb.ConnCase

  alias Schoolhub.Questions

  @create_attrs %{content: "some content"}
  @update_attrs %{content: "some updated content"}
  @invalid_attrs %{content: nil}

  def fixture(:qreply) do
    {:ok, qreply} = Questions.create_qreply(@create_attrs)
    qreply
  end

  describe "index" do
    test "lists all question_replies", %{conn: conn} do
      conn = get(conn, Routes.qreply_path(conn, :index))
      assert html_response(conn, 200) =~ "Listing Question replies"
    end
  end

  describe "new qreply" do
    test "renders form", %{conn: conn} do
      conn = get(conn, Routes.qreply_path(conn, :new))
      assert html_response(conn, 200) =~ "New Qreply"
    end
  end

  describe "create qreply" do
    test "redirects to show when data is valid", %{conn: conn} do
      conn = post(conn, Routes.qreply_path(conn, :create), qreply: @create_attrs)

      assert %{id: id} = redirected_params(conn)
      assert redirected_to(conn) == Routes.qreply_path(conn, :show, id)

      conn = get(conn, Routes.qreply_path(conn, :show, id))
      assert html_response(conn, 200) =~ "Show Qreply"
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.qreply_path(conn, :create), qreply: @invalid_attrs)
      assert html_response(conn, 200) =~ "New Qreply"
    end
  end

  describe "edit qreply" do
    setup [:create_qreply]

    test "renders form for editing chosen qreply", %{conn: conn, qreply: qreply} do
      conn = get(conn, Routes.qreply_path(conn, :edit, qreply))
      assert html_response(conn, 200) =~ "Edit Qreply"
    end
  end

  describe "update qreply" do
    setup [:create_qreply]

    test "redirects when data is valid", %{conn: conn, qreply: qreply} do
      conn = put(conn, Routes.qreply_path(conn, :update, qreply), qreply: @update_attrs)
      assert redirected_to(conn) == Routes.qreply_path(conn, :show, qreply)

      conn = get(conn, Routes.qreply_path(conn, :show, qreply))
      assert html_response(conn, 200) =~ "some updated content"
    end

    test "renders errors when data is invalid", %{conn: conn, qreply: qreply} do
      conn = put(conn, Routes.qreply_path(conn, :update, qreply), qreply: @invalid_attrs)
      assert html_response(conn, 200) =~ "Edit Qreply"
    end
  end

  describe "delete qreply" do
    setup [:create_qreply]

    test "deletes chosen qreply", %{conn: conn, qreply: qreply} do
      conn = delete(conn, Routes.qreply_path(conn, :delete, qreply))
      assert redirected_to(conn) == Routes.qreply_path(conn, :index)
      assert_error_sent 404, fn ->
        get(conn, Routes.qreply_path(conn, :show, qreply))
      end
    end
  end

  defp create_qreply(_) do
    qreply = fixture(:qreply)
    %{qreply: qreply}
  end
end
