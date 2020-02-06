var elm_ethereum_ports = require('elm-ethereum-ports');
const {Elm} = require('Main');
var node = document.getElementById("elmapp")

//getNetwork is too slow for the initial load so creates and error
//in console but can be ignored for now
//currently ignoring the errors in console since the Elm init appears to work
//eventually
window.addEventListener('load', function () {

    if (typeof web3 !== 'undefined') {
      console.log("in addEventListener")
        web3.version.getNetwork(function (e, networkId) {
            app = Elm.Main.init({flags: parseInt(networkId), node: node});
            //elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
            //elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
            ethereum.enable();
        });
    } else {
        app = Elm.Main.init({flags: 0, node: node});
        console.log("Metamask not detected.");
    }
});


//Original from putbackinsrclater (not complex)
// var elm_ethereum_ports = require('elm-ethereum-ports');

// //const {Elm} = require('./Challenge');
// const {Elm} = require('Challenge');
// var node = document.getElementById("elmapp")

// //getNetwork is too slow for the initial load so creates and error
// //in console but can be ignored for now
// //currently ignoring the errors in console since the Elm init appears to work
// //eventually
// window.addEventListener('load', function () {

//     if (typeof web3 !== 'undefined') {
//       console.log("in addEventListener")
//         web3.version.getNetwork(function (e, networkId) {
//             app = Elm.Challenge.init({flags: parseInt(networkId), node: node});
//             elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
//             elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
//             ethereum.enable();
//         });
//     } else {
//         app = Elm.Challenge.init({flags: 0, node: node});
//         console.log("Metamask not detected.");
//     }
// });


