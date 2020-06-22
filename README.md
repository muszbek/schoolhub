# Schoolhub

A communication platform targeted for facilitating education courses.

Contains a server and a client application both written in Elixir.
For the moment there is no graphical front-end, coming later.

## Installation

Both in the `./schoolhub_server` and `./schoolhub_client` folders:

```
mix deps.get
```

## Running

Launch the MongooseIM server, Postgres and images for integration tests:

```
docker-compose up
```

Launch the Schoolhub server:

```
cd schoolhub_server  
iex -S mix
```

Launch the Schoolhub client:

```
cd schoolhub_client  
iex -S mix
```

