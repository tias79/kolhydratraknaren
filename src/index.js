import './normalize.css';
import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import foods from './foods.json';
import robotofontLicense from './licenses/robotofont';
import PouchDB from 'pouchdb';
import PouchDBUpsert from 'pouchdb-upsert';
import SqlLiteAdapter from 'pouchdb-adapter-cordova-sqlite';

if (window.cordova) {
  document.addEventListener('deviceready', init, false)
} else {
  init()
}

function init() {
  const getAdapter = () =>
      process.env.NODE_ENV === 'test'
          ? 'memory'
          : window.cordova && window.sqlitePlugin
          ? 'cordova-sqlite'
          : undefined

  PouchDB.plugin(PouchDBUpsert).plugin(SqlLiteAdapter);

  const crypto = window.crypto || window.msCrypto;
  const getRandomInts = (n) => {
    const randInts = new Uint32Array(n);
    crypto.getRandomValues(randInts);
    return Array.from(randInts);
  };
  const randInts = getRandomInts(1);

  let db = new PouchDB('foods4.db', {adapter: getAdapter() });
  db.allDocs({
    include_docs: true
  }).then(function (docs) {
    docs.rows.forEach(row => foods.foods.push( row.doc ))

    let app = Elm.Main.init({
      node: document.getElementById('root'),
      flags: {
        foods: foods,
        robotoFontLicense: robotofontLicense,
        uuidInitialSeed: randInts[0]
      }
    });
    app.ports.sendFoodPort.subscribe(function(json) {
      db.upsert(json.id, (doc) => (json));
    });

  }).catch(function (err) {
    console.error(err);
  });

  registerServiceWorker();
}