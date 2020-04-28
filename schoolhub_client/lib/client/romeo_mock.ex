defmodule Client.RomeoMock do
  @moduledoc """
  A mock implementation of the dependenci Romeo for testing.
  """
end


defmodule Client.RomeoMock.Connection do
  @mock_host "localhost"
  @mock_user "test_user"
  @mock_pw "test_pw"

  @mock_conn :connection_stub
  @mock_resource "51D7588DEFD5CA6C1587-494607-823546"
  
  def start_link([jid: @mock_user <> "@localhost", password: @mock_pw]) do
    Process.send(self(), {:resource_bound, @mock_resource}, [])
    Process.send(self(), :connection_ready, [])
    {:ok, @mock_conn}
  end

  def send(@mock_conn, stanza) do
    if stanza == Romeo.Stanza.presence() do

      reply = %Romeo.Stanza.Presence{from: %Romeo.JID{full: @mock_user <>"@" <> @mock_host <> "/" <> @mock_resource, resource: @mock_resource, server: @mock_host, user: @mock_user}, id: nil, show: "", status: "", to: %Romeo.JID{full: @mock_user <> "@" <> @mock_host <> "/" <> @mock_resource, resource: @mock_resource, server: @mock_host, user: @mock_user}, type: nil, xml: {:xmlel, "presence", [{"from", @mock_user <> "@" <> @mock_host<> "/" <> @mock_resource}, {"to", @mock_user <> "@" <> @mock_host <> "/" <> @mock_resource}, {"xml:lang", "en"}], []}}

      Process.send(self(), {:stanza, reply}, [])
    end
    
    :ok
  end
end

defmodule Client.RomeoMock.Stanza do

  def presence() do
    {:xmlel, "presence", [], []}
  end
end
