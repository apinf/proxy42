import React, { Component } from 'react';

import { hot } from 'react-hot-loader/root';

import { Provider } from "react-redux";
import store from "../store";
import { HashRouter as Router } from "react-router-dom";

// Theming
import { Grommet } from 'grommet';
import { grommet } from "grommet/themes";

// Routing
import { Route, Link } from "react-router-dom";

import {NewApiForm, ViewApi} from '../apis';
import {AllApisTable} from "./AllApisTable.js";
import Home from './Home';
import ViewAPI from './ViewAPI';
import EditAPI from './EditAPI';

@hot
class App extends Component {
  render() {
    return (
      <Grommet theme={grommet}>
        <Provider store={store}>
          <Router>
            <Route path="/" exact component={Home} />
            <Route path="/apis" exact component={AllApisTable} />
            <Route path="/apis/new" exact component={NewApiForm} />
            <Route path="/apis/:id" exact component={ViewAPI} />
            <Route path="/apis/:id/edit" exact component={EditAPI} />
          </Router>
        </Provider>,
      </Grommet>
    );
  }
}

export default App;
