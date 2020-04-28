defmodule Schoolhub.RomeoMock do
  @moduledoc """
  A mock implementation of the dependenci Romeo for testing.
  """
end


defmodule Schoolhub.RomeoMock.Connection do

  @mock_admin "admin"
  @mock_host "10.3.0.2"

  @mock_user "new_user"
  @mock_pw "new_pw"

  @mock_conn :connection_stub
  @mock_resource "51D7588DEFD5CA6C1587-494607-823546"
  
  def start_link([jid: @mock_admin <> "@" <> @mock_host, password: _mock_admin_pw]) do
    Process.send(self(), {:resource_bound, @mock_resource}, [])
    Process.send(self(), :connection_ready, [])
    {:ok, @mock_conn}
  end

  def send(@mock_conn, stanza) do
    if stanza == Schoolhub.RomeoMock.Stanza.set_inband_register(@mock_user, @mock_pw) do
      
      reply = "<iq from='" <> @mock_admin <> "@" <> @mock_host <> "' to='" <>
	@mock_admin <> "@" <> @mock_host <> "/" <> @mock_resource <>
	"' id='7929' type='result'><query xmlns='jabber:iq:register'><username>" <>
	@mock_user <> "</username><password>" <> @mock_pw <> "</password></query></iq>"
      
      Process.send(self(), {:stanza, reply}, [])
    end
    
    :ok
  end
end


defmodule Schoolhub.RomeoMock.Stanza do

  @mock_id "319a"

  def set_inband_register(username, password) do
    {:xmlel, "iq", [{"type", "set"}, {"id", @mock_id}],
     [
       {:xmlel, "query", [{"xmlns", "jabber:iq:register"}],
	[
	  {:xmlel, "username", [], [xmlcdata: username |> to_string()]},
	  {:xmlel, "password", [], [xmlcdata: password |> to_string()]}
	]}
     ]}
  end
end
