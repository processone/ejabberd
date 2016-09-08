defmodule Ejabberd.Config.Attr do
  @moduledoc """
  Module used to work with the attributes parsed from
  an elixir block (do...end).

  Contains functions for extracting attrs from a block
  and validation.
  """

  @type attr :: {atom(), any()}

  @attr_supported [
    active:
      [type: :boolean, default: true],
    git:
      [type: :string, default: ""],
    name:
      [type: :string, default: ""],
    opts:
      [type: :list, default: []],
    dependency:
      [type: :list, default: []]
  ]

  @doc """
  Takes a block with annotations and extracts the list
  of attributes.
  """
  @spec extract_attrs_from_block_with_defaults(any()) :: [attr]
  def extract_attrs_from_block_with_defaults(block) do
    block
    |> extract_attrs_from_block
    |> put_into_list_if_not_already
    |> insert_default_attrs_if_missing
  end

  @doc """
  Takes an attribute or a list of attrs and validate them.

  Returns a {:ok, attr} or {:error, attr, cause} for each of the attributes.
  """
  @spec validate([attr]) :: [{:ok, attr}] | [{:error, attr, atom()}]
  def validate(attrs) when is_list(attrs), do: Enum.map(attrs, &valid_attr?/1)
  def validate(attr), do: validate([attr]) |> List.first

  @doc """
  Returns the type of an attribute, given its name.
  """
  @spec get_type_for_attr(atom()) :: atom()
  def get_type_for_attr(attr_name) do
    @attr_supported
    |> Keyword.get(attr_name)
    |> Keyword.get(:type)
  end

  @doc """
  Returns the default value for an attribute, given its name.
  """
  @spec get_default_for_attr(atom()) :: any()
  def get_default_for_attr(attr_name) do
    @attr_supported
    |> Keyword.get(attr_name)
    |> Keyword.get(:default)
  end

  # Private API

  # Given an elixir block (do...end) returns a list with the annotations
  # or a single annotation.
  @spec extract_attrs_from_block(any()) :: [attr] | attr
  defp extract_attrs_from_block({:__block__, [], attrs}), do: Enum.map(attrs, &extract_attrs_from_block/1)
  defp extract_attrs_from_block({:@, _, [attrs]}), do: extract_attrs_from_block(attrs)
  defp extract_attrs_from_block({attr_name, _, [value]}), do: {attr_name, value}
  defp extract_attrs_from_block(nil), do: []

  # In case extract_attrs_from_block returns a single attribute,
  # then put it into a list. (Ensures attrs are always into a list).
  @spec put_into_list_if_not_already([attr] | attr) :: [attr]
  defp put_into_list_if_not_already(attrs) when is_list(attrs), do: attrs
  defp put_into_list_if_not_already(attr), do: [attr]

  # Given a list of attributes, it inserts the missing attribute with their
  # default value.
  @spec insert_default_attrs_if_missing([attr]) :: [attr]
  defp insert_default_attrs_if_missing(attrs) do
    Enum.reduce @attr_supported, attrs, fn({attr_name, _}, acc) ->
      case Keyword.has_key?(acc, attr_name) do
        true -> acc
        false -> Keyword.put(acc, attr_name, get_default_for_attr(attr_name))
      end
    end
  end

  # Given an attribute, validates it and return a tuple with
  # {:ok, attr} or {:error, attr, cause}
  @spec valid_attr?(attr) :: {:ok, attr} | {:error, attr, atom()}
  defp valid_attr?({attr_name, param} = attr) do
    case Keyword.get(@attr_supported, attr_name) do
      nil -> {:error, attr, :attr_not_supported}
      [{:type, param_type} | _] -> case is_of_type?(param, param_type) do
        true -> {:ok, attr}
        false -> {:error, attr, :type_not_supported}
      end
    end
  end

  # Given an attribute value and a type, it returns a true
  # if the value its of the type specified, false otherwise.

  # Usefoul for checking if an attr value respects the type
  # specified for the annotation.
  @spec is_of_type?(any(), atom()) :: boolean()
  defp is_of_type?(param, type) when type == :boolean and is_boolean(param), do: true
  defp is_of_type?(param, type) when type == :string and is_bitstring(param), do: true
  defp is_of_type?(param, type) when type == :list and is_list(param), do: true
  defp is_of_type?(param, type) when type == :atom and is_atom(param), do: true
  defp is_of_type?(_param, type) when type == :any, do: true
  defp is_of_type?(_, _), do: false
end
