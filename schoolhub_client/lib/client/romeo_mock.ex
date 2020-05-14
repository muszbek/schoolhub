defmodule Client.RomeoMock do
  @moduledoc """
  A mock implementation of the dependenci Romeo for testing.
  """
end


defmodule Client.RomeoMock.Connection do
  @mock_host "10.3.0.2"
  @mock_user "test_user"
  @mock_user_new "test_user_new"
  @mock_user_teacher "test_user_teacher"
  @mock_user_student "test_user_student"
  @mock_pw "test_pw"
  @mock_admin "admin"
  @mock_admin_pw "admin"

  @mock_conn :connection_stub
  @mock_resource "51D7588DEFD5CA6C1587-494607-823546"
  
  def start_link([jid: @mock_user <> "@" <> @mock_host <> "", password: @mock_pw]) do
    start_mock()
  end
  def start_link([jid: @mock_admin <> "@" <> @mock_host <> "", password: @mock_admin_pw]) do
    start_mock()
  end
  def start_link([jid: @mock_user_teacher <> "@" <> @mock_host <> "", password: @mock_pw]) do
    start_mock()
  end
  def start_link([jid: @mock_user_student <> "@" <> @mock_host <> "", password: @mock_pw]) do
    start_mock()
  end
  def start_link([jid: @mock_user_new <> "@" <> @mock_host <> "", password: @mock_pw]) do
    start_mock()
  end

  def send(@mock_conn, stanza) do
    if stanza == Romeo.Stanza.presence() do

      reply = %Romeo.Stanza.Presence{from: %Romeo.JID{full: @mock_user <>"@" <> @mock_host <> "/" <> @mock_resource, resource: @mock_resource, server: @mock_host, user: @mock_user}, id: nil, show: "", status: "", to: %Romeo.JID{full: @mock_user <> "@" <> @mock_host <> "/" <> @mock_resource, resource: @mock_resource, server: @mock_host, user: @mock_user}, type: nil, xml: {:xmlel, "presence", [{"from", @mock_user <> "@" <> @mock_host<> "/" <> @mock_resource}, {"to", @mock_user <> "@" <> @mock_host <> "/" <> @mock_resource}, {"xml:lang", "en"}], []}}

      Process.send(self(), {:stanza, reply}, [])
    end
    
    :ok
  end

  defp start_mock() do
    Process.send(self(), {:resource_bound, @mock_resource}, [])
    Process.send(self(), :connection_ready, [])
    {:ok, @mock_conn}
  end
end

defmodule Client.RomeoMock.Stanza do

  def presence() do
    {:xmlel, "presence", [], []}
  end
end
