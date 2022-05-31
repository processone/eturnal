### Kubernetes Kustomize example

This is a Kustomize example, which can be used to deploy eturnal into Kubernetes. This example is **not** ready to use and must be adjusted to your needs.

The example provides a reverse proxy example with `traefik` and TLS certificate manager `cert-manager`.

To deploy eturnal use the following command:

    kubectl apply -k ./overlay/dev

To remove eturnal use:

    kubectl delete -k ./overlay/dev

Adjust the parameters to your needs in `./overlay/dev`. Especially also consider blacklisting Kubernetes' networks to prevent attacks into your cluster.
