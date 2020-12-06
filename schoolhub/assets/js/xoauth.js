export function Mechanism() {
}

// Conform to the SASL lib's expectations
Mechanism.Mechanism = Mechanism;


Mechanism.prototype.name = 'X-OAUTH';
Mechanism.prototype.clientFirst = true;


Mechanism.prototype.response = function (cred) {
    return cred.password;
};

Mechanism.prototype.challenge = function (chal) {
};

