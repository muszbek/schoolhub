const authUrl = window.location.origin.concat("/auth");
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
	    const form = document.forms[0];
	    addResult(form, 'result', authResult);
	    addResult(form, 'username', creds.username);
	    form.submit();
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
