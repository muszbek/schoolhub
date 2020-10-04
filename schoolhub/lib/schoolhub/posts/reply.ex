defmodule Schoolhub.Posts.Reply do
  use Ecto.Schema
  import Ecto.Changeset

  alias Schoolhub.Accounts.User
  alias Schoolhub.Posts.Post

  schema "post_replies" do
    field :content, :string
    belongs_to :post, Post,
      foreign_key: :parent_post
    belongs_to :user, User,
      foreign_key: :creator

    timestamps()
  end

  @doc false
  def changeset(reply, attrs) do
    reply
    |> cast(attrs, [:content, :parent_post, :creator])
    |> validate_required([:content, :parent_post, :creator])
    |> foreign_key_constraint(:parent_post)
    |> foreign_key_constraint(:creator)
  end
end
