# Elixir unit tests

## Running Elixir unit tests

You can run Elixir unit tests with command:

make quicktest

You need to have ejabberd compile with Elixir and tools enabled.

## Troubleshooting test

To help with troubleshooting Elixir tests, we have added a special macro in ejabberd `logger.hrl` include file: ?EXUNIT_LOG

To use this, in test file:

1. in `setup_all, add:

    ```
    Application.start(:logger)
    ```

2. Enable log capture for the test you want to analyse by adding
   `capture_log` tag before test implementation:

   ```
   @tag capture_log: true
   ```

In the ejabberd code, if `logger.hrl` is included, you can code adds a
EXUNIT_LOG macro:

    ?EXUNIT_LOG("My debug log:~p ~p", [Arg1, Arg2])
