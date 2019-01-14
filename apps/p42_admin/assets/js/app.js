// import css from "../css/app.css";
// import "phoenix_html"

import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';

const rootElement = document.getElementById("root");
ReactDOM.render(<App />, rootElement);

if(module.hot){
  window.jpm = module;
    // module.hot.accept('./components/App', () => {
    //     console.log("App HMR")
    //     const UpdatedApp = require('./components/App').default;
    //     ReactDOM.render(<UpdatedApp />, rootElement);
    // });
}
