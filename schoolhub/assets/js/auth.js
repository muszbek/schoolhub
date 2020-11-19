import "regenerator-runtime/runtime"

document.getElementById("login_button").addEventListener("click", login, false);

const authUrl = window.location.origin.concat("/auth");
const csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

const {client, xml} = require("@xmpp/client");
const tokenNS = 'erlang-solutions.com:xmpp:token-auth:0';

function login() {
    console.log("Logging in");

    var username = document.getElementById("username").value;
    var password = document.getElementById("password").value;
    var creds = {"username": username,
		 "password": password};

    var sasl = require('saslmechanisms');
    var sasl_scram = require('sasl-scram-sha-1');
    var factory = new sasl.Factory();
    factory.use(sasl_scram);
    var mech = factory.create(['SCRAM-SHA-1']);

    authenticate(mech, creds)
	.then(authResult => {
	    const form = document.forms[0];
	    addResult(form, 'result', authResult);
	    addResult(form, 'username', creds.username);
	    
	    authXMPP(creds);
	    
	    //form.submit();
	})
};

function authenticate(mech, creds) {
    var clientFirst = mech.response(creds);
    var clientFirstData = JSON.stringify({"data": clientFirst});
    
    var authPromise = sendAuth(clientFirstData)
	.then(httpResponse => httpResponse.text())
	.then(responseJson => JSON.parse(responseJson).data)
	.then(serverFirst => {
	    var clientFinal = mech.challenge(serverFirst).response(creds);
	    var clientFinalData = JSON.stringify({"data": clientFinal});
	    return sendAuth(clientFinalData);
	})
	.then(httpResponse => httpResponse.text())
	.then(responseJson => JSON.parse(responseJson).data)
	.then(serverFinal => {
	    return mech.challenge(serverFinal).response(creds);
	})
	.catch(error => {
	    return error;
	})

    return authPromise;
};

function sendAuth(body_data) {
    return sendHttp(authUrl, body_data);
}

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

function addResult(form, key, authResult) {
    const hiddenField = document.createElement('input');
    hiddenField.type = 'hidden';
    hiddenField.name = key;
    hiddenField.value = authResult;

    form.appendChild(hiddenField);
};


function authXMPP(creds) {
    var host = document.getElementById("host").value;
    var domain = document.getElementById("domain").value;
    var jid = creds.username + '@' + domain

    const xmpp = client({
	service: 'ws://' + host + ':5280/ws-xmpp',
	domain: domain,
	username: creds.username,
	password: creds.password
    });

    xmpp.on("error", (err) => {
	console.error(err);
    });

    xmpp.start().catch(console.error);
    
    waitForEventWithTimeout(xmpp, 'online', 2000)
	.then(() => {
	    console.log("session started");
	    
	});
    
};


// https://gist.github.com/simongregory/2c60d270006d4bf727babca53dca1f87
function waitForEventWithTimeout(emitter, eventName, timeout) {
    return new Promise((resolve, reject) => {
        let timer;

        function listener(data) {
            clearTimeout(timer);
            emitter.off(eventName, listener);
            resolve(data);
        }

        emitter.on(eventName, listener);
        timer = setTimeout(() => {
            emitter.off(eventName, listener);
            reject(new Error("timeout waiting for " + eventName));
        }, timeout);
    });
};
