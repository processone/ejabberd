defmodule Ejabberd.Config.OptsFormatter do
  @moduledoc """
  Module for formatting options parsed into the format
  ejabberd uses.
  """

  alias Ejabberd.Config.EjabberdModule

  @doc """
  Takes a keyword list with keys corresponding to
  the keys requested by the ejabberd config (ex: modules: mods)
  and formats them to be correctly evaluated by ejabberd.

  Look at how Config.get_ejabberd_opts/0 is constructed for
  more informations.
  """
  @spec format_opts_for_ejabberd([{atom(), any()}]) :: list()
  def format_opts_for_ejabberd(opts) do
    opts
    |> format_attrs_for_ejabberd
  end

  defp format_attrs_for_ejabberd(opts) when is_list(opts),
    do: Enum.map opts, &format_attrs_for_ejabberd/1

  defp format_attrs_for_ejabberd({:listeners, mods}),
    do: {:listen, format_listeners_for_ejabberd(mods)}

  defp format_attrs_for_ejabberd({:modules, mods}),
    do: {:modules, format_mods_for_ejabberd(mods)}

  defp format_attrs_for_ejabberd({key, opts}) when is_atom(key),
    do: {key, opts}

  defp format_mods_for_ejabberd(mods) do
    Enum.map mods, fn %EjabberdModule{module: mod, attrs: attrs} ->
      {mod, attrs[:opts]}
    end
  end

  defp format_listeners_for_ejabberd(mods) do
    Enum.map mods, fn %EjabberdModule{module: mod, attrs: attrs} ->
      Keyword.put(attrs[:opts], :module, mod)
    end
  end
end
