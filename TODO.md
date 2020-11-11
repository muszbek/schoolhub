
- handle persistent data in postgres volume
- check mam common test why database is not cleaned
- react front-end?

- grades to XMPP
- upload files
- implement more course functionality

- disable course operations on deactivated courses

- fix docker volume sharing, no rebuild for the code every time
-- production build with docker servers
-- local ct with local servers
-- distributed ct with docker servers, preceded by local rebuild
-- server exunit

- whitelist plug macro make it read the test config
- mongooseim whitelist access from HAProxy
- romeo catch error when server is down


# Phoenix

https between server and haproxy (once server is dockerized)

mass add users to course
chat

more sophisticated self user api

course disable
course members can delete edit their own messages


# launch:
in ssl:
sudo chown 999:999 schoolhub.key
sudo chmod 0600 schoolhub.key

sudo apt-get install inotify-tools

# deploy:
fix mongooseim version, check with new config locations
