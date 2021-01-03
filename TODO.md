
- handle persistent data in postgres volume
- check mam common test why database is not cleaned

- grades to XMPP
- implement more course functionality

- disable course operations on deactivated courses

- whitelist plug macro make it read the test config
- mongooseim whitelist access from HAProxy


# Phoenix

https between server and haproxy (once server is dockerized)

mass add users to course

more sophisticated self user api

course disable
course members can delete edit their own messages
course members displayed in alphabetical order

enable secure session data in Endpoint.ex


# launch:
in ssl:
sudo chown 999:999 schoolhub.key
sudo chmod 0600 schoolhub.key

sudo apt-get install inotify-tools
