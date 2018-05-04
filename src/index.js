import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Main.embed(document.getElementById('root'), { 
    foods: [{id: 12, name: "banan", gPercentage: 0.22, gDefault: 70}] });

registerServiceWorker();
