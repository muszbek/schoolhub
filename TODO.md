
- grades to XMPP
- implement more course functionality

- disable course operations on deactivated courses

- mongooseim whitelist access from HAProxy


# Phoenix
mass add users to course

course members displayed in alphabetical order

embedded links should open in new tab instead of redirect current

force certain tags for questions? empty tag is invalid?
fix error when delete user - foreign key constraint - how to fix?

what is the EHLO email restart message? related to openDKIM, maybe testing that it will work

view tests

minimal style changes
pictures to courses
liveview filtering
follow descriptive

remove picture

mongooseim kubernetes clustering:
Failed clustering mongooseim@schoolhub-mongooseim-56d8f4866f-gqqs6 with mongooseim@schoolhub-mongooseim-56d8f4866f-1

domain name should not be hardcoded in kustomization in the tls certificate path

- remove containerports from not exposed ports? only after I can test functionality
- dynamically generate new service with all pod?
- explore other ways to proxy directly to specific pods via subdomain? otherwise services...
- anyway how to access internal network by ip?

nslookup schoolhub
telnet schoolhub-instance-1.schoolhub.default.svc.cluster.local 1080

ports in uri are changed by haproxy regex?

k3d image import phx_server:v0.2.2 mongooseim/mongooseim:3.7.1 postgres:13.3 haproxy:2.4 boky/postfix:v3.3.0 busybox:1.33
