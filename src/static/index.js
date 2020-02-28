var elm_ethereum_ports = require('elm-ethereum-ports');
const { Elm } = require('Main');
var node = document.getElementById("elmapp");
var setOfFlags = {};

window.addEventListener('load', function () {
    if (typeof ethereum !== 'undefined') {
        // 'ethereum' means supports EIP-1102 injected Ethereum providers.
        window.web3 = new Web3(ethereum);
        web3.version.getNetwork(function (e, networkId) {
            app = window.ports.init(Elm.Main.init({ flags: setOfFlags, node: node }));
        });
    } else if (typeof web3 !== 'undefined') {
        // Supports legacy injected Ethereum providers.
        window.web3 = new Web3(web3.currentProvider);
        web3.version.getNetwork(function (e, networkId) {
            app = window.ports.init(Elm.Main.init({ flags: setOfFlags, node: node }));
        });
    } else {
        // Your preferred fallback.
        app = window.ports.init(Elm.Main.init({ flags: setOfFlags, node: node }));
        console.log("Metamask not detected. Using local provider");
        window.web3 = new Web3(new Web3.providers.HttpProvider('http://localhost:8545'));
    }
});

//multi-port initialize
window.ports = {
    init: (app) =>
        (
            app.ports.outgoing.subscribe(({ action, data }) =>

                actions[action]
                    ? actions[action](data)
                    : console.warn(`I didn't recognize action "${action}".`)
            )
            //these are the only ports that are being used currently ...
            , elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3)
            , elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3)
        )
}


// maps outgoing actions to functions!
const actions = {
    'LOG': (message) =>
        console.log(`From Elm:`, message)
}