var elm_ethereum_ports = require('elm-ethereum-ports');
const {Elm} = require('Main');
var node = document.getElementById("elmapp")
var dataFromJS = "Data from JS";
var setOfFlags = {networkid : Int16Array, comment : String};

window.addEventListener('load', function () {

    if (typeof web3 !== 'undefined') {
        web3.version.getNetwork(function (e, networkId) {
            //app = window.ports.init(Elm.Main.init({flags: parseInt(networkId), node: node}));
            setOfFlags.networkid = parseInt(networkId);
            setOfFlags.comment = " new flags can be added to the setOfFlags record in index.js";
            app = window.ports.init(Elm.Main.init({flags: setOfFlags, node: node}));
            ethereum.enable();
        });
    } else {
        //app = window.ports.init(Elm.Main.init({flags: parseInt(networkId), node: node}));
        setOfFlags.networkid = 0;
        setOfFlags.comment = " Metamask must be installed before this app can be used";
        app = window.ports.init(Elm.Main.init({flags: setOfFlags, node: node}));
        console.log("Metamask not detected.");
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
                    ,
                        app.ports.incoming.send(dataFromJS)
                     , elm_ethereum_ports.txSentry(app.ports.txOut, app.ports.txIn, web3)
                     , elm_ethereum_ports.walletSentry(app.ports.walletSentry, web3)
                  )
              }


// maps outgoing actions to functions!
const actions = {
  'LOG': (message) =>
    console.log(`From Elm:`, message)
}