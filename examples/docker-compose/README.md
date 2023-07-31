### Example Docker Compose File

Use this Docker Compose file to deploy eturnal in a Docker environment. To have a fully working eturnal server, you should mount an `eturnal.yml` [configuration file](https://github.com/processone/eturnal/blob/master/config/eturnal.yml) into the container as well as consider to use the `network_mode: "host"` option.

To deploy with Compose, use:

`docker-compose up -d` or `docker compose up -d` - depending on your Compose version.

To remove eturnal, use:

`docker-compose down` or `docker compose down` - depending on your Compose version.
