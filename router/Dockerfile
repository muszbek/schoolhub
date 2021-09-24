FROM bitwalker/alpine-elixir-phoenix:1.11.3

RUN apk --no-cache --update add \
	postgresql-client && \
    rm -rf /var/cache/apk/*

COPY . /opt/app

RUN mix deps.get && \
    MIX_ENV=prod mix deps.compile

RUN cd assets && \
    npm install

RUN cd assets/ && \
    npm run deploy && \
    cd - && \
    MIX_ENV=prod mix phx.digest

ENTRYPOINT ["./entrypoint.sh"]
CMD ["./launcher.sh"]
