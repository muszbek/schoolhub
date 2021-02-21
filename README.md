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

Have the address `schoolhub.com` evaluate to `127.0.0.1` in `/etc/hosts`

### In production:

In root directory:

```
sudo ./get_cert.sh MYDOMAIN
```

Your host has to be visible on the internet under MYDOMAIN, on ports 443, 5285, (80)

## Running

In the root directory, launch the MongooseIM server, Postgres, HAProxy, and the Phoenix server:

```
docker-compose up
```

Now you can access the webpage:

```
https://schoolhub.com

https://MYDOMAIN
```
