//var elm_ethereum_ports = require('elm-ethereum-ports');
// const {Elm} = require('./Components.EnteredResult');
// var node = document.getElementById("elm")

// var elm_ethereum_ports = require('elm-ethereum-ports');

//var Elm = require( '../elm/Main' );

//On load, listen to Elm!
// window.addEventListener('load', _ => {
//   //alert ("Hello World!")
//     if (typeof web3 !== 'undefined') {
//       console.log("in addEventListener")
//         web3.version.getNetwork(function (e, networkId) {
//             app = Elm.Components.EnteredResult.init({flags: parseInt(networkId), node: node});
//             elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
//             elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
//             ethereum.enable();
//         });
//     } else {
//         app = Elm.Components.EnteredResult.init({flags: 0, node: node});
//         console.log("Metamask not detected.");
//     }

//window.addEventListener('load', function () {

//     if (typeof web3 !== 'undefined') {
      
//         web3.version.getNetwork(function (e, networkId) {
//           alert("hi theeeer")
//             app = Elm.Main.fullscreen(parseInt(networkId || 0));
//             elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
//             elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
//         });
//     } else {
//         app = Elm.Main.fullscreen(0);
//         console.log("Metamask not detected.");
//     }


//});

window.ports = {
    init: (app) =>
      app.ports.outgoing.subscribe(({ action, data }) =>
        actions[action]
          ? actions[action](data)
          : console.warn(`I didn't recognize action "${action}".`)
        )
    
      // app.ports.txOut.subscribe(({ txIn, web3 }) =>
      //   actions[action]
      //     ? actions[action](data)
      //     : console.warn(`I didn't recognize action "${action}".`)
      //   )
}
     



//   app.ports.toMain.subscribe(function(args) {
//   ipcRenderer.send("to-main", args);
// });

    // ipcRenderer.on("from-main", function(_, args) {
    //   app.ports.fromMain.send(args);
    // });


  
//})

//elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
//elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);

// maps actions to functions!
const actions = {
  'LOG': (message) =>
    console.log(`From Elm:`, message)
}

//getNetwork is too slow for the initial load so creates and error
//in console but can be ignored for now
//currently ignoring the errors in console since the Elm init appears to work
//eventually
// window.addEventListener('load', function () {

//     if (typeof web3 !== 'undefined') {
//       console.log("in addEventListener")
//         web3.version.getNetwork(function (e, networkId) {
//             app = Elm.EnteredResult.init({flags: parseInt(networkId), node: node});
//             elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
//             elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
//             ethereum.enable();
//         });
//     } else {
//         app = Elm.EnteredResult.init({flags: 0, node: node});
//         console.log("Metamask not detected.");
//     }
// });



