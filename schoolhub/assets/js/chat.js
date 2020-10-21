const XMPP = require('stanza');
const host = document.getElementById("host").value;
const domain = document.getElementById("domain").value;
const username = document.getElementById("username").value;
const self = document.getElementById("self").value;

const client = XMPP.createClient({
    jid: self + '@' + domain,
    password: self,
    server: host,
    
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
    console.log("got message: " + msg.body);
    var from = msg.from.substr(0, msg.from.indexOf('/'));
    var to_print = from + " -> " + msg.body;
    print(to_print);
});

client.connect();


send_msg = function() {
    console.log("sending message...");
    var jid = username + '@' + domain;
    var msg = document.getElementById("sendbox").value;
    if (msg == "")
	return;
    
    client.sendMessage({
	to: jid,
	body: msg
    });
    
    var to_print = jid + " -> " + msg;
    print(to_print);
    document.getElementById("sendbox").value = "";
};

function print(msg) {
    var print_box = document.getElementById("chatbox")
    var full_text = print_box.value;
    var new_text = full_text + "\n" + msg;
    print_box.value = new_text;
};