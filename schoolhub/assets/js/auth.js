const authUrl = window.location.origin.concat("/auth");
const resultUrl = window.location.origin.concat("/sessions");
const csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

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
	    var result = JSON.stringify({"credential": creds});
	    sendResult(result);
	    console.log(authResult);
	})
    
};

function authenticate(mech, creds) {
    var clientFirst = mech.response(creds);
    var clientFirstData = JSON.stringify({"data": clientFirst});
    
    var authPromise = sendAuth(clientFirstData)
	.then(httpResponse => httpResponse.text())
	.then(serverFirst => {
	    var clientFinal = mech.challenge(serverFirst).response(creds);
	    var clientFinalData = JSON.stringify({"data": clientFinal});
	    return sendAuth(clientFinalData);
	})
	.then(httpResponse => httpResponse.text())
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

function sendResult(body_data) {
    return sendHttp(resultUrl, body_data);
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
