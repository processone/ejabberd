defmodule Ejabberd.Config.AttrTest do
  use ExUnit.Case, async: true

  alias Ejabberd.Config.Attr

  test "extract attrs from single line block" do
    block = quote do
      @active false
    end

    block_res = Attr.extract_attrs_from_block_with_defaults(block)
    assert {:active, false} in block_res
  end

  test "extract attrs from multi line block" do
    block = quote do
      @active false
      @opts [http: true]
    end

    block_res = Attr.extract_attrs_from_block_with_defaults(block)
    assert {:active, false} in block_res
    assert {:opts, [http: true]} in block_res
  end

  test "inserts correctly defaults attr when missing in block" do
    block = quote do
      @active false
      @opts [http: true]
    end

    block_res = Attr.extract_attrs_from_block_with_defaults(block)

    assert {:active, false} in block_res
    assert {:git, ""} in block_res
    assert {:name, ""} in block_res
    assert {:opts, [http: true]} in block_res
    assert {:dependency, []} in block_res
  end

  test "inserts all defaults attr when passed an empty block" do
    block = quote do
    end

    block_res = Attr.extract_attrs_from_block_with_defaults(block)

    assert {:active, true} in block_res
    assert {:git, ""} in block_res
    assert {:name, ""} in block_res
    assert {:opts, []} in block_res
    assert {:dependency, []} in block_res
  end

  test "validates attrs and returns errors, if any" do
    block = quote do
      @not_supported_attr true
      @active "false"
      @opts [http: true]
    end

    block_res =
      block
      |> Attr.extract_attrs_from_block_with_defaults
      |> Attr.validate

    assert {:ok, {:opts, [http: true]}} in block_res
    assert {:ok, {:git, ""}} in block_res
    assert {:error, {:not_supported_attr, true}, :attr_not_supported} in block_res
    assert {:error, {:active, "false"}, :type_not_supported} in block_res
  end

  test "returns the correct type for an attribute" do
    assert :boolean == Attr.get_type_for_attr(:active)
    assert :string == Attr.get_type_for_attr(:git)
    assert :string == Attr.get_type_for_attr(:name)
    assert :list == Attr.get_type_for_attr(:opts)
    assert :list == Attr.get_type_for_attr(:dependency)
  end

  test "returns the correct default for an attribute" do
    assert true == Attr.get_default_for_attr(:active)
    assert "" == Attr.get_default_for_attr(:git)
    assert "" == Attr.get_default_for_attr(:name)
    assert [] == Attr.get_default_for_attr(:opts)
    assert [] == Attr.get_default_for_attr(:dependency)
  end
end
