import "regenerator-runtime/runtime"
import { Mechanism } from './xoauth.js';

document.getElementById("send_button").addEventListener("click", send_msg, false);

const tokenUrl = window.location.origin.concat("/token");
const csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

const {client, xml} = require("@xmpp/client");
const {decode} = require("@xmpp/base64");
const debug = require("@xmpp/debug");
const host = document.getElementById("host").value;
const domain = document.getElementById("domain").value;
const username = document.getElementById("username").value;
const self = document.getElementById("self").value;
const token = document.getElementById("token").value;

const xmpp = client({
    service: 'wss://' + host + ':5285/ws-xmpp',
    domain: domain,
    username: self,
    // sasl will base64 encode the token automatically, but it is received already encoded
    password: decode(token)
});
//debug(xmpp, true);

addXoauth(xmpp);

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

xmpp.on('element', (element) => {
    if (!element.is('success')) return;

    console.log("New token received");
    var newToken = element.text();
    sendNewToken(newToken);
});

xmpp.on('stanza', (stanza) => {
    //console.log("got stanza: " + stanza.toString());
    if (!stanza.is('message')) return;

    var from_long = stanza.attrs.from;
    var from = from_long.substr(0, from_long.indexOf('/'));
    var from_just_name = from_long.substr(0, from_long.indexOf('@'));

    if (from_just_name != username) return;
    
    var to_print = from + " -> " + stanza.getChildText("body");
    print(to_print);
});

console.log("Logging in XMPP...")
xmpp.start().catch(console.error);


function addXoauth(xmpp) {
    const mech = Mechanism;
    const {sasl} = xmpp;
    sasl.use(mech);
    xmpp.sasl = sasl
};

function sendNewToken(token) {
    if (token == "") {
	console.warn('Token received upon auth is empty! No new token stored!');
	return;
    }
    
    var httpData = JSON.stringify({"refresh_token": token});
    sendHttp(tokenUrl, httpData)
	.then(httpResponse => httpResponse.text())
	.then(response => {
	    console.log(response);
	});
};

function sendHttp(targetUrl, body_data) {
    return fetch(targetUrl, {
	method: "POST",
	headers: {
	    "Content-Type": "application/json",
	    "X-CSRF-Token": csrfToken
	},
	body: body_data
    })
};

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
