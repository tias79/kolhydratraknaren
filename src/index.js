import './normalize.css';
import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import foods from './foods.json';
import robotofontLicense from './licenses/robotofont';

Main.embed(document.getElementById('root'), [foods, robotofontLicense]);

registerServiceWorker();