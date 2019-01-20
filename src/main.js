import App from './App.purs';
import { Application } from 'pixi.js';

const app = new Application(128, 128, { backgroundColor: 0x1099bb });
document.body.appendChild(app.view);

// create a new Sprite from an image path
const bunny = PIXI.Sprite.fromImage('./assets/bunny.png');

// center the sprite's anchor point
bunny.anchor.set(0.5);

// move the sprite to the center of the screen
bunny.x = app.screen.width / 2;
bunny.y = app.screen.height / 2;

app.stage.addChild(bunny);
let state = App.initialState;
let timePassed = 0;
app.ticker.add(function(delta) {
  timePassed += delta;
  let { value0: newState, value1: outState } = App.main(state)(timePassed);
  if (state !== newState) {
    console.log('state change');
    state = newState;
  }
  bunny.x = outState.position.x;
  bunny.y = outState.position.y;
});

window.app = App;

// vanilla hot module reloading
// @see https://webpack.js.org/guides/hot-module-replacement/
// const onReload = () => {
//   App.main(20)
// }

// console.log(App.main(20))

// if (module.hot) {
//   onReload()
// } else {
//   onReload()
// }
