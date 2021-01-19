FROM bitwalker/alpine-elixir-phoenix:latest AS phx-builder

RUN apk update && \
    apk --no-cache --update add postgresql-client

COPY . /opt/app

RUN mix deps.get && \
    MIX_ENV=prod mix deps.compile

RUN cd assets && \
    npm install

RUN cd assets/ && \
    npm run deploy && \
    cd - && \
    mix do compile, phx.digest

ENTRYPOINT ["./entrypoint.sh"]
CMD ["./launcher.sh"]