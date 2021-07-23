import "regenerator-runtime/runtime"

document.getElementById("login_button").addEventListener("click", login, false);

const form = document.forms[0];
const internalHost = document.getElementById("internal_host").value;
const authUrl = window.location.origin.concat(internalHost).concat("/auth");
const csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

const {client, xml} = require("@xmpp/client");
const debug = require("@xmpp/debug");
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
	    addResult(form, 'result', authResult);
	    addResult(form, 'username', creds.username);
	    
	    return maybeAuthXMPP(authResult, creds);
	})
	.then(() => {
	    form.submit();
	});
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
	    "X-CSRF-Token": csrfToken,
	    "X-Internal-Host": internalHost
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

function maybeAuthXMPP(authResult, creds) {
    if (authResult == "authenticated") {
	return authXMPP(creds);
    }
    else {
	// return an empty promise to make this call then-able without authXMPP
	return new Promise(resolve => {
	    resolve();
	});
    }
};

function authXMPP(creds) {
    var host = document.getElementById("xmpp_url").value;
    var domain = document.getElementById("domain").value;
    var jid = creds.username + '@' + domain

    var xmpp = client({
	service: host,
	domain: domain,
	username: creds.username,
	password: creds.password
    });
    //debug(xmpp, true);

    addScram(xmpp);

    xmpp.on("error", (err) => {
	console.error(err);
    });
    
    console.log("HTTP SCRAM authentication passed... Now connecting XMPP");
    xmpp.start().catch(console.error);
    
    var tokenPromise = waitForEventWithTimeout(xmpp, 'online', 2000)
	.then(async () => {
	    console.log("session started");
	    var iq = requestTokenIq(jid);
	    var {iqCaller} = xmpp;
	    return await iqCaller.request(iq, 1000).catch(console.error);
	})
	.then(response => {
	    return new Promise(resolve => {
		takeTokens(response, form);
		console.log("tokens added");
		resolve();
	    })
	});

    return tokenPromise;
};

function addScram(xmpp) {
    // fix import of scram for @xmpp
    const mech = require('sasl-scram-sha-1')
    const {sasl} = xmpp;
    sasl.use(mech);
    xmpp.sasl = sasl
}

function requestTokenIq(jid) {
    return xml(
	"iq",
	{type: "get", to: jid},
	xml("query", {xmlns: tokenNS})
    );
};

function takeTokens(tokenIq, form) {
    var tokens = tokenIq.getChild("items");
    var accessToken = tokens.getChildText("access_token");
    var refreshToken = tokens.getChildText("refresh_token");
    addResult(form, "access_token", accessToken);
    addResult(form, "refresh_token", refreshToken);
}


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
