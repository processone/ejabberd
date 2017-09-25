# Ejabberd cluster with docker compose

This example uses [dnsdocker](https://github.com/tonistiigi/dnsdock) to discover other nodes and setup a multi-master cluster.

Build the ejabberd cluster image:

```bash
git clone https://github.com/rroemhild/docker-ejabberd.git
cd docker-ejabberd/examples/docker-compose-cluster
docker-compose build
```

Start dnsdocker and the first ejabberd node:

```bash
docker-compose up -d
```

Wait until the first ejabberd node is up and running `docker-compose logs ejabberd`, then add some ejabberd nodes to the cluster:

```bash
docker-compose scale ejabberd=4
```
