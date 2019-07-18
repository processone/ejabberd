# Docker database images to run ejabberd tests

## Starting databases

You can start the Docker environment with Docker Compose, from ejabberd repository root.

The following command will launch MySQL, PostgreSQL, Redis and Riak, and keep the console
attached to it.

```
mkdir test/docker/db/mysql/data
mkdir test/docker/db/postgres/data
mkdir test/docker/db/riak/data
(cd test/docker; docker-compose up)
```

You can stop all the databases with CTRL-C.

## Running tests

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
rm -rf test/docker/db/riak/data
```
