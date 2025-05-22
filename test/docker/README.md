# Docker database images to run ejabberd tests

## Starting databases

You can start the Docker environment with Docker Compose, from ejabberd repository root.

The following command will launch MySQL, MSSQL, PostgreSQL, Redis and keep the console
attached to it.

```
mkdir test/docker/db/mysql/data
mkdir test/docker/db/postgres/data
(cd test/docker; docker compose up)
```

You can stop all the databases with CTRL-C.

## Creating database for MSSQL

The following commands will create the necessary login, user and database, will grant rights on the database in MSSQL and create the ejabberd schema:

```
docker exec ejabberd-mssql /opt/mssql-tools18/bin/sqlcmd -U SA -P ejabberd_Test1 -S localhost -i /initdb_mssql.sql -C
docker exec ejabberd-mssql /opt/mssql-tools18/bin/sqlcmd -U SA -P ejabberd_Test1 -S localhost -d ejabberd_test -i /mssql.sql -C
```

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
(cd test/docker; docker compose down)
```

If you want to clean the data, you can remove the data volumes after the `docker-compose down` command:

```
docker volume rm ejabberd-mysqldata
docker volume rm ejabberd-mssqldata
docker volume rm ejabberd-pgsqldata
```
