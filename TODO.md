
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

[NOTICE]   (1) : haproxy version is 2.4.0-6cbbecf
[NOTICE]   (1) : path to executable is /usr/local/sbin/haproxy
[ALERT]    (1) : parsing [/usr/local/etc/haproxy/haproxy.cfg:22] : The 'reqrep' directive is not supported anymore since HAProxy 2.1. Use 'http-request replace-path', 'http-request replace-uri' or 'http-request replace-header' instead.
[ALERT]    (1) : Error(s) found in configuration file : /usr/local/etc/haproxy/haproxy.cfg
[ALERT]    (1) : Fatal errors found in configuration.

ports in uri are changed by haproxy regex?
