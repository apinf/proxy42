import css from "../css/app.css"
import "phoenix_html"

import React, { Component } from 'react';
import ReactDOM from 'react-dom';
import App from './components/App';

const rootElement = document.getElementById("root");
ReactDOM.render(
  <App />,
  rootElement
);
