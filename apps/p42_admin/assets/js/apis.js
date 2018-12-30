import React, { Component } from 'react';
import { Form, FormField, SimpleButton, Button, Collapsible, TextInput, Box, Grommet, DataTable, Text } from 'grommet'

fetch("http://httpbin.org/headers")
    .then(res => res.json())
    .then(res => console.log(res))

let APIURL = "http://84.20.150.158:4001/apis"
let getAPI = (id) => fetch(`${APIURL}/${id}`)
    .then(res => res.json())

export class NewApiForm extends Component {

    handleChange = (e) => {
        const target = e.target;
        const value = target.value;
        const name = target.name;
        this.setState({
            [name]: value,
        });
    }

    onSubmit = (e) => {
        e.preventDefault();
        console.log(this.state)
        const form = {
            hostname: this.state.hostname,
            frontend_prefix: this.state.frontend_prefix,
            backend_prefix: this.state.backend_prefix,
            servers: [this.state.hostname],
            strategy: 'random',
            additional_headers: '',
            rate_limit: 5,
            auth_config: {
                strategy: this.state.auth_strategy,
                config_id: "bearer-auth"
            }

        };
        fetch('http://84.20.150.158:4001/apis',
              {
                  method: 'post',
                  body: JSON.stringify(form),
                  headers: {
                      "Accept": "application/json",
                      "Content-Type": "application/json"}
              }).then((resp) => console.log(resp));
    }

    render() {
        return (
                <Box>
                <FormField>
                <TextInput
            name="frontend_prefix"
            placeholder="From"
            onChange={(e) => {this.handleChange(e)}}
                />
                <TextInput
            name="backend_prefix"
            placeholder="To"
            onChange={(e) => {this.handleChange(e)}}
                />
                <TextInput
            name="auth_strategy"
            placeholder="Auth Strategy"
            onChange={(e) => {this.handleChange(e)}}
                />
                <TextInput
            name="hostname"
            placeholder="Hostname"
            onChange={(e) => {this.handleChange(e)}}
                />
                <Button
            primary
            color="#000"
            label="Submit"
            onClick={(e) =>
                     {this.onSubmit(e)}}
                />
                </FormField>
                </Box>)
    }
}

export class ViewApi extends Component {
    render() {
        return (
                <div>ViewApi</div>
        )}
}
