const targetUrl = window.location.origin.concat("/auth");
const csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

login = function() {
    console.log("Logging in");
    
    var creds = {"username": "tmuszbek",
		 "password": "super_secret"};

    var sasl = require('saslmechanisms');
    var factory = new sasl.Factory();
    factory.use(require('sasl-scram-sha-1'));
    var mech = factory.create(['SCRAM-SHA-1']);
    
    var clientFirst = mech.response(creds);
    var clientFirstData = JSON.stringify({"data": clientFirst});
    
    sendHttp(clientFirstData)
	.then(httpResponse => httpResponse.text())
	.then(serverFirst => {
	    var clientFinal = mech.challenge(serverFirst).response(creds);
	    var clientFinalData = JSON.stringify({"data": clientFinal});
	    console.log(clientFinalData);
	    return sendHttp(clientFinalData);
	})
	.then(httpResponse => httpResponse.text())
	.then(serverFinal => {
	    console.log(serverFinal);
	})
};

function sendHttp(body_data) {
    return fetch(targetUrl, {
	method: "POST",
	headers: {
	    "Content-Type": "application/json",
	    "X-CSRF-Token": csrfToken
	},
	body: body_data
    })
};
