# Docker database images to run ejabberd tests

## Starting databases

You can start the Docker environment with Docker Compose, from ejabberd repository root.

The following command will launch MySQL, PostgreSQL, Redis and keep the console
attached to it.

```
mkdir test/docker/db/mysql/data
mkdir test/docker/db/postgres/data
(cd test/docker; docker-compose up)
```

You can stop all the databases with CTRL-C.

## Running tests

Before running the test, you can ensure there is no running instance of Erlang common test tool. You can run the following
command, especially if all test are skipped with an `eaddrinuse` error:

```
pkill -9 ct_run
```

You can run tests with (from ejabberd repository root):

```
make test
```

## Cleaning up the test environment

You can fully clean up the environment with:

```
(cd test/docker; docker-compose down)
```

If you want to clean the data, you can remove the data directories after the `docker-compose down` command:

```
rm -rf test/docker/db/mysql/data
rm -rf test/docker/db/postgres/data
```
