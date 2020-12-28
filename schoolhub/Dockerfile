FROM bitwalker/alpine-elixir-phoenix:latest AS phx-builder

RUN apk update && \
    apk --no-cache --update add postgresql-client

COPY . /opt/app

RUN mix do deps.get, deps.compile

RUN cd assets && \
    npm install

# RUN cd assets/ && \
#    npm run deploy
#    cd - && \
#    mix do compile, phx.digest

CMD ["/opt/app/entrypoint.sh"]