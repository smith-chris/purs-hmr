import App from './App.purs'

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
