const XMPP = require('stanza');
//import * as XMPP from 'stanza';

const client = XMPP.createClient({
    jid: 'admin@10.3.2.1',
    password: 'admin',

    // If you have a .well-known/host-meta.json file for your
    // domain, the connection transport config can be skipped.
    transports: {
        websocket: 'ws://10.3.2.1:5280/ws-xmpp',
        bosh: 'http://10.3.2.1:5280/http-bind'
    }
});


client.on('session:started', () => {
    console.log("session started");
    client.getRoster();
    client.sendPresence();
});

client.on('chat', msg => {
    console.log(msg.body);
    //client.sendMessage({
    //    to: msg.from,
    //    body: 'You sent: ' + msg.body
    //});
});

client.connect();

send_msg = function() {
    console.log("button pushed");
    var jid = 'admin@10.3.2.1';
    client.sendMessage({
	to: jid,
	body: "this is a message"
    });
};

console.log("script read");
