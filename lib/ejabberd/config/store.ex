defmodule Ejabberd.Config.Store do
  @moduledoc """
    Module used for storing the modules parsed from
    the configuration file.

    Example:
      - Store.put(:modules, mod1)
      - Store.put(:modules, mod2)

      - Store.get(:modules) :: [mod1, mod2]

    Be carefoul: when retrieving data you get them
    in the order inserted into the store, which normally
    is the reversed order of how the modules are specified
    inside the configuration file. To resolve this just use
    a Enum.reverse/1.
  """

  @name __MODULE__

  def start_link do
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def put(key, val) do
    Agent.update @name, &Map.update(&1, key, [val], fn coll ->
      [val | coll]
    end)
  end

  def get(key) do
    Agent.get @name, &Map.get(&1, key, [])
  end
end
