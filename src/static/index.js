var elm_ethereum_ports = require('elm-ethereum-ports');
const {Elm} = require('Main');
var node = document.getElementById("elmapp")

window.addEventListener('load', function () {

    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            app = window.ports.init(Elm.Main.init({flags: parseInt(networkId), node: node}));
            //elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3);
            //elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3);
            ethereum.enable();
        });
    } else {
        app = window.ports.init(Elm.Main.init({flags: parseInt(networkId), node: node}));
        console.log("Metamask not detected.");
    }
});

//multi-port initialize (outgoing at least) works below ...
window.ports = {
                init: (app) =>
                  (
                        app.ports.outgoing.subscribe(({ action, data }) =>

                        actions[action]
                          ? actions[action](data)
                          : console.warn(`I didn't recognize action "${action}".`)
                      )
                    //,
                        //app.ports.incoming.send("Hey Elm!")
                      // app.ports.incoming.send((data) =>
                      //       ("hellow from js!!")
                        // actions[action]
                        //   ? actions[action](data)
                        //   : console.warn(`I didn't recognize action "${action}".`)
                      //)
                  )
              }

//simple version from docs
//app.ports.incoming.send("Hey Elm!")

// maps actions to functions!
const actions = {
  'LOG': (message) =>
    console.log(`From Elm:`, message)
}