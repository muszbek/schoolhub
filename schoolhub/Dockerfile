FROM bitwalker/alpine-elixir-phoenix:latest AS phx-builder

EXPOSE 4000
EXPOSE 4001

COPY . /opt/app

RUN mix do deps.get, deps.compile

RUN cd assets && \
    npm install
