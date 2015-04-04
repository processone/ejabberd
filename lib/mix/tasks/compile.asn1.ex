defmodule Mix.Tasks.Compile.Asn1 do
  use Mix.Task
  alias Mix.Compilers.Erlang
  
  @recursive true
  @manifest ".compile.asn1"

  @moduledoc """
  Compile ASN.1 source files.
  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the source directory with a .erl extension and generate a .hrl file.
  You can force compilation regardless of modification times by passing
  the `--force` option.
  ## Command line options
    * `--force` - forces compilation regardless of modification times
  ## Configuration
    * `:asn1_paths` - directories to find asn1 files. Defaults to `["asn1"]`.
  """

  @doc """
  Runs this task.
  """
  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:asn1_paths] || ["asn1"]
    dest_paths    = project[:erlc_paths]
    mappings     = Enum.zip(source_paths, dest_paths)
    options      = project[:asn1_options] || []

    Erlang.compile(manifest(), mappings, :asn1, :erl, opts[:force], fn
      input, output ->
        options = options ++ [:noobj, outdir: Erlang.to_erl_file(Path.dirname(output))]
        :asn1ct.compile(Erlang.to_erl_file(input), options)
    end)
  end

  @doc """
  Returns ASN.1 manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    Erlang.clean(manifest())
  end
end
