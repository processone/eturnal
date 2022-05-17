### Example docker compose file

Use this docker compose file to deploy eturnal in a docker environment. To have a fully working eturnal server, you should mount a `eturnal.yml` config file into the container as well as consider to use the `network_mode: "host"` setting.

To deploy with compose, use:

`docker-compose up -d` or `docker compose up -d` - depending on your compose version.

To remove eturnal, use:

`docker-compose down` or `docker compose down` - depending on your compose version.
