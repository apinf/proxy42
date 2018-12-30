import css from "../css/app.css"
import "phoenix_html"

import ReactDOM from 'react-dom';
import { Provider } from "react-redux";
import store from "./store";

import React, { Component } from 'react';
import { Grommet, Tab, Tabs } from 'grommet'

import {NewApiForm, ViewApi} from './apis';
import {AllApisTable} from "./components/AllApisTable.js";

class App extends Component {
  render() {
    return (
      <Grommet plain>
      <Tabs flex>
      <Tab title="Create API">
        <NewApiForm />
      </Tab>
      <Tab title="API Table">
      <AllApisTable />
      </Tab>
      </Tabs>
      </Grommet>
    );
  }
}

export default App;

const rootElement = document.getElementById("root");
ReactDOM.render(
  <Provider store={store}>
    <App />
  </Provider>,
  rootElement
);
