# Docker database images to run ejabberd tests

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

You can fully clean up the environment with:

```
(cd test/docker; docker-compose down)
```

If you want to clean the data, you can remove the data directories after the `docker-compose down` command:

```
mkdir test/docker/db/mysql/data
mkdir test/docker/db/postgres/data
mkdir test/docker/db/riak/data
