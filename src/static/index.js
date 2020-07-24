
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
        //this window.ports.init step is currently necessary
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
            //js is subscribed to Elm's 'outgoing' port:
            app.ports.outgoing.subscribe(function({ action, data }) {
                console.log(`From Elm at port:`, data)
                if (data == "eth_requestAccounts") {
                    ethereum.request({ method: 'eth_requestAccounts' });
                } else {
                    console.log('Nothing happened');
                }
            })
            , elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3)
            , elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3)
            //,
            //, elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, window.ethereum)
            //, elm_ethereum_ports.walletSentry(app.ports.walletSentry, window.ethereum)
        )
}