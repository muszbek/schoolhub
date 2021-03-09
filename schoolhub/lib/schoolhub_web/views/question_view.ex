defmodule SchoolhubWeb.QuestionView do
  use SchoolhubWeb, :view
  
  def render_tag(taglist \\ []) do
    Enum.join(taglist, " ")
  end
end
