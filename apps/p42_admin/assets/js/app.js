import css from "../css/app.css"
import "phoenix_html"

import ReactDOM from 'react-dom';
import React, { Component } from 'react';

import {NewApiForm, AllApisTable, ViewApi} from './apis';

class App extends Component {
  render() {
    return (
      <div className="App">
        <NewApiForm />
      <hr/><AllApisTable/><hr/>
        <ViewApi />
      </div>
    );
  }
}

export default App;

ReactDOM.render(<App />, document.getElementById('root'));
