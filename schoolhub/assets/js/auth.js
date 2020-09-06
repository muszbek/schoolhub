login = function() {
    console.log("Logging in");
    const targetUrl = window.location.origin.concat("/auth");
    var creds = {"username": "tmuszbek",
		 "password": "super_secret"};
    var csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;

    var sasl = require('saslmechanisms');
    var factory = new sasl.Factory();
    factory.use(require('sasl-scram-sha-1'));
    var mech = factory.create(['SCRAM-SHA-1']);
    var initial = mech.response(creds);
    var data = JSON.stringify({"data": initial})
    console.log(data);
    
    fetch(targetUrl, {
	method: "POST",
	headers: {
	    "Content-Type": "application/json",
	    "X-CSRF-Token": csrfToken
	},
	body: data
    })
	.then(responseData => {
	    console.log(responseData);
	})
};
