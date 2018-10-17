import App from './App.purs';

// vanilla hot module reloading
// @see https://webpack.js.org/guides/hot-module-replacement/
const onReload = () => {
  App.main({
    liftEffect: (a) => a(),
  });
};

if (module.hot) {
  onReload();
} else {
  onReload();
}
