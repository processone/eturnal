### Kubernetes kustomize example

This is a kustomize example, which can be used to deploy eturnal into kubernetes. This is example is **not** ready to use and must be adjusted to your needs.

The example provides a reverse proxy example with `traefik` and tls certificate manager `cert-manager`.

To deploy eturnal use the following command:

`kubectl apply -k ./overlay/dev`

To remove eturnal use:

`kubectl delete -k ./overlay/dev`

Adjust the parameters to your needs in `./overlay/dev`. Especially also consider blacklisting kubernetes' networks  to prevent attacks into your cluster.
