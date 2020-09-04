login = function() {
    console.log("Logging in");
    const targetUrl = window.location.origin.concat("/sessions");
    var data = {"user": {"username": "tmuszbek",
			 "password": "super_secret"}};
    var csrfToken = document.head.querySelector("[name~=csrf-token][content]").content;
    
    fetch(targetUrl, {
	method: "POST",
	headers: {
	    "Content-Type": "application/json",
	    "X-CSRF-Token": csrfToken
	},
	body: JSON.stringify(data)
    })
	.then(responseData => {
	    console.log(responseData);
	})
};
