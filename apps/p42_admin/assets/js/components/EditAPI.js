import { hot } from 'react-hot-loader/root';
import React, { Component } from 'react';
import { connect } from 'react-redux';

import { Link } from "react-router-dom";
import { reduxForm } from 'redux-form';
import { loadAPI, saveAPI } from "../actions";

import { SimpleButton, Button, Collapsible, TextInput, Box, Grommet, DataTable, Text } from 'grommet'
import FormField from './FormField'


const mapStateToProps = (state, ownProps) => {
    const apiId = ownProps.match.params.id;
    const api = state.cache.apis[apiId];
    if(api) { return { api, initialValues: api }; }
    else { return { api: {loaded: false}}; }
};
const mapDispatchToProps = { loadAPI, saveAPI };

@connect(mapStateToProps, mapDispatchToProps)
@reduxForm({ form: 'edit-api' })
@hot
export default class EditAPI extends Component {
    componentDidMount() {
        const id = this.props.match.params.id;
        if(!this.props.api.loaded) {
            this.props.loadAPI(id)
        }
    }

  render() {
      const { api, handleSubmit, reset, submit } = this.props;
    const id = this.props.match.params.id;
    return(
            <form onSubmit={handleSubmit(this.props.saveAPI)}>
            Editing API with id: {id}
        <FormField
        name="frontend_prefix"
          label="From"
          component={TextInput}
        />
        <FormField
        name="backend_prefix"
          label="To"
          component={TextInput}
        />
        <FormField
        name="auth_config.auth_strategy"
          label="Auth Strategy"
          component={TextInput}
        />
        <FormField
        name="hostname"
          label="Hostname"
          component={TextInput}
        />
        <Button onClick={reset} label="Undo changes" />
        <Button primary type="submit" label="Save API" />
      </form>
    )
  }
}
