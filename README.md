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

## Running

Back in the root directory, run this script to generate ssl certificates:

```
sudo ./dev_gen_ssl.sh
```

Launch the MongooseIM server, Postgres, HAProxy, and the Phoenix server:

```
docker-compose up
```

If in a development environment, have the address `schoolhub.com` evaluate to `127.0.0.1` in `/etc/hosts`

Now you can access the webpage:

```
http://schoolhub.com
https://schoolhub.com
```
