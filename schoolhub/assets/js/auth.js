const authUrl = window.location.origin.concat("/auth");
const csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

const XMPP = require('stanza');
const stanzas = new XMPP.JXT.Registry();
const requestTokenIq = stanzas.define({
    name: 'requestToken',
    element: 'query',
    path: 'iq.requestToken',
    namespace: 'erlang-solutions.com:xmpp:token-auth:0'
});


login = function() {
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

    var client = XMPP.createClient({
	jid: jid,
	password: creds.password,
	server: host,
    
	transports: {
            websocket: 'ws://' + host + ':5280/ws-xmpp',
            bosh: 'http://' + host + ':5280/http-bind'
	}
    });

    client.on('raw:outgoing', iq => {
	console.log(iq);
    });

    client.connect();
    
    var promise = waitForEventWithTimeout(client, 'session:started', 2000);
    promise.then(() => {
	console.log("session started");
	var myIq = stanzas.export('iq.requestToken', {});
	console.log(myIq.toString());
				  
	client.sendIQ({
	    to: jid,
	    type: 'get',
	    requestToken: {}
	}).then(result => {
	    console.log(result);
	})
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
