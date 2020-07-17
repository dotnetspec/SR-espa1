
var elm_ethereum_ports = require('elm-ethereum-ports');
const { Elm } = require('Main');
var node = document.getElementById("elmapp");
var setOfFlags = {};
//const web3 = new Web3(Web3.givenProvider || 'wss://rinkeby.infura.io/ws' || 'ws://localhost:8546', null, {});

const Web3 = require("web3");
const ethEnabled = () => {
  if (window.ethereum) {
    return true;
  }
  return false;
}

window.addEventListener('load', function () {
    if (typeof ethEnabled()) {
        console.log('MetaMask is installed!');
        console.log('Network version: ', window.ethereum.networkVersion);
        window.ethereum.autoRefreshOnNetworkChange = false;
        app = window.ports.init(Elm.Main.init({ flags: setOfFlags, node: node }));
    } 
    else {
        alert("Please install an Ethereum-compatible browser or extension like MetaMask to use this dApp!");
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
            // next line commented to enable walletSentry until we're ready to implement txSentry
            ,
            elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3)
            , elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3)
        )
}


// maps outgoing actions to functions!
const actions = {
    'LOG': (message) =>
        console.log(`From Elm:`, message)
}