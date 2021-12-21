# Schoolhub

A communication platform targeted for facilitating education courses.

Written in Phoenix.

All in docker containers, so make sure docker and docker-compose are installed.

## Installation

```
cd ./schoolhub
mix deps.get

cd ./assets
npm install
```

## Get SSL certificates

### In local development environment:

In the root directory:

```
sudo ./dev_gen_ssl.sh
```

Have the address `katedra.fr` evaluate to `127.0.0.1` in `/etc/hosts`

### In production:

In root directory:

```
sudo ./get_cert.sh MYDOMAIN
```

Your host has to be visible on the internet under MYDOMAIN, on ports 443, 1587, 5285, (80)

## Running a local instance

In the root directory, launch the MongooseIM server, Postgres, HAProxy, and the Phoenix server:

```
docker-compose up
```

## Running full infrastructure with Kubernetes

In root directory - build the Phoenix images:

```
docker-compose build phx-server
docker-compose -f docker-compose_router_test.yml build phx-server
```

Launch K3d and import the built images:

```
k3d cluster create -c kubernetes/headless/k3d_config.yaml
k3d image import phx_server:v0.4
k3d image import router_phx_server:v0.4
```

Launch the services:

```
sudo kubectl apply -k .
```


Now you can access the webpage:

```
https://katedra.fr

https://MYDOMAIN
```

![infographic-display](https://github.com/muszbek/schoolhub/blob/master/katedra_info.jpg)
