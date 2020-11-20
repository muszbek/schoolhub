# Schoolhub

A communication platform targeted for facilitating education courses.

Written in Phoenix.

## Installation

```
cd ./schoolhub
mix deps.get

cd ./assets
npm install
```

## Running

Launch the MongooseIM server, Postgres, HAProxy:

```
docker-compose up
```

Set up the Phoenix server:
```
cd ./schoolhub
mix ecto.migrate
mix run priv/repo/seeds.exs
mix phx.server
```

localhost:4000

(Proxy access and https deployment coming later...)
