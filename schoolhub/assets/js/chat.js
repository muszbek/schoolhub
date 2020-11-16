import "regenerator-runtime/runtime"

document.getElementById("send_button").addEventListener("click", send_msg, false);

const {client, xml} = require("@xmpp/client");
const host = document.getElementById("host").value;
const domain = document.getElementById("domain").value;
const username = document.getElementById("username").value;
const self = document.getElementById("self").value;

const xmpp = client({
    service: 'ws://' + host + ':5280/ws-xmpp',
    domain: domain,
    username: self,
    password: self
});

xmpp.on("error", (err) => {
    console.error(err);
});

xmpp.on('online', async (address) => {
    console.log("session started");
    await xmpp.send(xml("presence"));

    const message = xml(
	"message",
	{type: "chat", to: address},
	xml("body", {}, "hello world")
    );
    await xmpp.send(message);
});

xmpp.on('stanza', (stanza) => {
    console.log("got stanza: " + stanza.toString());
    if (!stanza.is('message')) return;

    var from_long = stanza.attrs.from;
    var from = from_long.substr(0, from_long.indexOf('/'));
    var to_print = from + " -> " + stanza.getChildText("body");
    print(to_print);
});

xmpp.start().catch(console.error);


async function send_msg() {
    console.log("sending message...");
    var jid = username + '@' + domain;
    var msg = document.getElementById("sendbox").value;
    if (msg == "")
	return;

    const message = xml(
	"message",
	{type: "chat", to: jid},
	xml("body", {}, msg)
    );
    await xmpp.send(message);

    var self_jid = self + '@' + domain;
    var to_print = self_jid + " -> " + msg;
    print(to_print);
    document.getElementById("sendbox").value = "";
};

function print(msg) {
    var print_box = document.getElementById("chatbox")
    var full_text = print_box.value;
    var new_text = full_text + "\n" + msg;
    print_box.value = new_text;
};
