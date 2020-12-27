defmodule SchoolhubWeb.Gettext do
  @moduledoc """
  A module providing Internationalization with a gettext-based API.

  By using [Gettext](https://hexdocs.pm/gettext),
  your module gains a set of macros for translations, for example:

      import SchoolhubWeb.Gettext

      # Simple translation
      gettext("Here is the string to translate")

      # Plural translation
      ngettext("Here is the string to translate",
               "Here are the strings to translate",
               3)

      # Domain-based translation
      dgettext("errors", "Here is the error message to translate")

  See the [Gettext Docs](https://hexdocs.pm/gettext) for detailed usage.
  """
  use Gettext, otp_app: :schoolhub

  def first_line(text) do
    [first | _rest] = String.split(text, "\n")
    [first | rest] = String.split(first, "\\")

    case rest do
      [] -> first
      _ -> first <> " ..."
    end
  end
end
