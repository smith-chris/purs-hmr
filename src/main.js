import App from './App.purs'
import { Application } from 'pixi.js'

var app = new Application(128, 128, { backgroundColor: 0x1099bb })
document.body.appendChild(app.view)

// create a new Sprite from an image path
var bunny = PIXI.Sprite.fromImage('./assets/bunny.png')

// center the sprite's anchor point
bunny.anchor.set(0.5)

// move the sprite to the center of the screen
bunny.x = app.screen.width / 2
bunny.y = app.screen.height / 2

app.stage.addChild(bunny)

// vanilla hot module reloading
// @see https://webpack.js.org/guides/hot-module-replacement/
const onReload = () => {
  App.main(20)
}

console.log(App.main(20))

if (module.hot) {
  onReload()
} else {
  onReload()
}
