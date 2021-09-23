FROM erlang:23-alpine AS phoenix

## based on image elixir

# elixir expects utf8.
ENV ELIXIR_VERSION="v1.11.3" \
	LANG=C.UTF-8

RUN set -xe \
	&& ELIXIR_DOWNLOAD_URL="https://github.com/elixir-lang/elixir/archive/${ELIXIR_VERSION}.tar.gz" \
	&& ELIXIR_DOWNLOAD_SHA256="d961305e893f4fe1a177fa00233762c34598bc62ff88b32dcee8af27e36f0564" \
	&& buildDeps=' \
		ca-certificates \
		curl \
		make \
	' \
	&& apk add --no-cache --virtual .build-deps $buildDeps \
	&& curl -fSL -o elixir-src.tar.gz $ELIXIR_DOWNLOAD_URL \
	&& echo "$ELIXIR_DOWNLOAD_SHA256  elixir-src.tar.gz" | sha256sum -c - \
	&& mkdir -p /usr/local/src/elixir \
	&& tar -xzC /usr/local/src/elixir --strip-components=1 -f elixir-src.tar.gz \
	&& rm elixir-src.tar.gz \
	&& cd /usr/local/src/elixir \
	&& make install clean \
	&& apk del .build-deps


## based on image bitwalker/alpine-elixir-phoenix

# Install NPM
RUN \
    mkdir -p /opt/app && \
    chmod -R 777 /opt/app && \
    apk update && \
    apk --no-cache --update add \
      bash \
      git \
      inotify-tools \
      postgresql-client \
      nodejs \
      npm && \
    npm install npm -g --no-progress && \
    update-ca-certificates --fresh && \
    rm -rf /var/cache/apk/*

# Add local node module binaries to PATH
ENV PATH=./node_modules/.bin:$PATH

# Ensure latest versions of Hex/Rebar are installed on build
ONBUILD RUN mix do local.hex --force, local.rebar --force

WORKDIR /opt/app


## own image
FROM phoenix

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
