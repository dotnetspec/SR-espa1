//import Web3 from 'web3';
//var Web3 = require('web3');
//var Eth = require('web3-eth');
var elm_ethereum_ports = require('elm-ethereum-ports');
const { Elm } = require('Main');
var node = document.getElementById("elmapp");
var setOfFlags = {};
//const web3 = new Web3(Web3.givenProvider || 'wss://rinkeby.infura.io/ws' || 'ws://localhost:8546', null, {});

//import detectEthereumProvider from '@metamask/detect-provider';

// if (typeof window.ethereum !== 'undefined') {
//     console.log('MetaMask is installed!');
//     console.log('Network version: ', window.ethereum.networkVersion);
//   }

const Web3 = require("web3");
const ethEnabled = () => {
  if (window.ethereum) {
    //window.web3 = new Web3(window.ethereum);
    //ethereum.enable();
    window.ethereum.eth_requestAccounts();
    window.ethereum.autoRefreshOnNetworkChange = false;
    
    return true;
  }
  return false;
}


const ethereumButton = document.querySelector('.enableEthereumButton');

ethereum.request({ method: 'eth_requestAccounts' });

window.addEventListener('load', function () {
    if (typeof ethereum !== 'undefined') {
        ethereumButton.addEventListener('click', () => {
            //Will Start the metamask extension
            //alert('clicked')
            //window.ethereum.request({ method: 'eth_requestAccounts' });
          });
    //if (typeof ethEnabled()) {
        // 'ethereum' means supports EIP-1102 injected Ethereum providers.
        //window.web3 = new Web3(ethereum);
        //web3.eth.net.isListening();
        console.log('MetaMask is installed!');
        console.log('Network version: ', window.ethereum.networkVersion);
        window.ethereum.request({ method: 'eth_requestAccounts' });
        //web3.version.getNetworkAndAccount(function (e, networkId) {
        //web3.eth.net.getId(function (e, networkId) {
        app = window.ports.init(Elm.Main.init({ flags: setOfFlags, node: node }));
        //});
    // } else if (typeof web3 !== 'undefined') {
    //     // Supports legacy injected Ethereum providers.
    //     window.web3 = new Web3(web3.currentProvider);
    //     web3.version.getNetwork(function (e, networkId) {
    //         app = window.ports.init(Elm.Main.init({ flags: setOfFlags, node: node }));
    //     });
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