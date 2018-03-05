import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'), { 
    foods: [{id: 12, name: "banan"}] });

registerServiceWorker();
