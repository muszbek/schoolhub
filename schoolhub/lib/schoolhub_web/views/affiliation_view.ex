defmodule SchoolhubWeb.AffiliationView do
  use SchoolhubWeb, :view

  alias Schoolhub.Accounts

  def get_user(user_id) do
    Accounts.get_user!(user_id)
  end
end
