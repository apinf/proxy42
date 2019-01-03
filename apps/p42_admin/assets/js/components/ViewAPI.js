import React, { Component } from 'react';
import { connect } from 'react-redux';

import { Link } from "react-router-dom";
import { loadAPI } from "../actions";

@connect(
  (state, ownProps) => {
    const apiId = ownProps.match.params.id
    const api = state.cache.apis[apiId];
    if(api) { return { api } }
    else { return { api: {loaded: false}}}
  },
    { loadAPI })

export default class ViewAPI extends Component {
  componentDidMount() {
    const id = this.props.match.params.id;
    if(!this.props.api.loaded) {
      this.props.loadAPI(id)
    }
  }

  render(){
    const { api } = this.props
    return  <section>
          Viewing API: {api.id}
          <hr/>
          {JSON.stringify(api, null, 2)}
          <hr/>
          {api.loaded &&
      <table>
        <tbody>
          <tr><td>From</td><td>{api.frontend_prefix}</td></tr>
          <tr><td>To</td><td>{api.backend_prefix}</td></tr>
          <tr><td>Auth Strategy</td><td>{api.auth_config.strategy}</td></tr>
          <tr><td>Hostname</td><td>{api.hostname}</td></tr>
        </tbody>
           </table>
          }
      <Link to={`/apis/${api.id}/edit`}>Edit this API</Link>
    </section>;
  }
}

