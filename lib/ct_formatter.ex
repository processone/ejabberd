defmodule ExUnit.CTFormatter do
  @moduledoc false

  use GenEvent

  import ExUnit.Formatter, only: [format_time: 2, format_test_failure: 5,
                                  format_test_case_failure: 5]

  def init(opts) do
    file = File.open! "exunit.log", [:append]
    # We do not print filter in log file as exclusion of test with tag
    # pending: true is always done
    config = %{
      file: file,
      seed: opts[:seed],
      trace: opts[:trace],
      colors: Keyword.put_new(opts[:colors], :enabled, false),
      width: 80,
      tests_counter: 0,
      failures_counter: 0,
      skipped_counter: 0,
      invalids_counter: 0
    }
    {:ok, config}
  end

  def handle_event({:suite_started, _opts}, config) do
    {:ok, config}
  end

  def handle_event({:suite_finished, run_us, load_us}, config) do
    print_suite(config, run_us, load_us)
    File.close config[:file]
    :remove_handler
  end

  def handle_event({:test_started, %ExUnit.Test{} = test}, config) do
    if config.tests_counter == 0, do: IO.binwrite config[:file], "== Running #{test.case} ==\n\n"
    {:ok, config}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: nil} = _test}, config) do
    IO.binwrite config[:file], "."
    {:ok, %{config | tests_counter: config.tests_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:skip, _}} = _test}, config) do
    {:ok, %{config | tests_counter: config.tests_counter + 1,
                     skipped_counter: config.skipped_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:invalid, _}} = _test}, config) do
    IO.binwrite config[:file], "?"
    {:ok, %{config | tests_counter: config.tests_counter + 1,
                     invalids_counter: config.invalids_counter + 1}}
  end

  def handle_event({:test_finished, %ExUnit.Test{state: {:failed, failures}} = test}, config) do
    formatted = format_test_failure(test, failures, config.failures_counter + 1,
                                    config.width, &formatter(&1, &2, config))
    print_failure(formatted, config)
    print_logs(test.logs)

    {:ok, %{config | tests_counter: config.tests_counter + 1,
                     failures_counter: config.failures_counter + 1}}
  end

  def handle_event({:case_started, %ExUnit.TestCase{}}, config) do
    {:ok, config}
  end

  def handle_event({:case_finished, %ExUnit.TestCase{state: nil}}, config) do
    {:ok, config}
  end

  def handle_event({:case_finished, %ExUnit.TestCase{state: {:failed, failures}} = test_case}, config) do
    formatted = format_test_case_failure(test_case, failures, config.failures_counter + 1,
                                         config.width, &formatter(&1, &2, config))
    print_failure(formatted, config)
    {:ok, %{config | failures_counter: config.failures_counter + 1}}
  end

  ## Printing

  defp print_suite(config, run_us, load_us) do
    IO.binwrite config[:file], "\n\n"
    IO.binwrite config[:file], format_time(run_us, load_us)
    IO.binwrite config[:file], "\n\n"

    # singular/plural
    test_pl = pluralize(config.tests_counter, "test", "tests")
    failure_pl = pluralize(config.failures_counter, "failure", "failures")

    message =
      "#{config.tests_counter} #{test_pl}, #{config.failures_counter} #{failure_pl}"
      |> if_true(config.skipped_counter > 0, & &1 <> ", #{config.skipped_counter} skipped")
      |> if_true(config.invalids_counter > 0, & &1 <> ", #{config.invalids_counter} invalid")

    cond do
      config.failures_counter > 0 -> IO.binwrite config[:file], message
      config.invalids_counter > 0 -> IO.binwrite config[:file], message
      true                        -> IO.binwrite config[:file], message
    end

    IO.binwrite config[:file], "\nRandomized with seed #{config.seed}\n\n\n\n"
  end

  defp if_true(value, false, _fun), do: value
  defp if_true(value, true, fun), do: fun.(value)

  defp print_failure(formatted, config) do
    IO.binwrite config[:file], "\n"
    IO.binwrite config[:file], formatted
    IO.binwrite config[:file], "\n"
  end

  defp formatter(_,  msg, _config),
    do: msg

  defp pluralize(1, singular, _plural), do: singular
  defp pluralize(_, _singular, plural), do: plural

  defp print_logs(""), do: nil

  defp print_logs(output) do
    indent = "\n     "
    output = String.replace(output, "\n", indent)
    IO.puts(["     The following output was logged:", indent | output])
  end
end
