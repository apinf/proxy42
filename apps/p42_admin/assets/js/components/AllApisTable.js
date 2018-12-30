import React, {Component} from 'react';
import { DataTable, Box, Text, Button, Grommet } from 'grommet';
import { connect } from "react-redux";
import { NewApiForm } from "../apis"

let APIURL = "http://84.20.150.158:4001/apis"
let getAPIs = () => fetch(`${APIURL}`)
    .then(res => res.json())

export class AllApisTable extends Component {

    constructor(props){
        super(props)
        this.state = {columns: [
            {property: 'id', primary: true, header: <Text>ID</Text>,
             render: datum => datum.id.slice(-4) },
            {property: 'frontend_prefix', header: <Text>From</Text>},
            {property: 'backend_prefix', header: <Text>To</Text>},
            {property: 'auth_config', header: <Text>Strategy</Text>,
             render: datum => datum.auth_config.strategy},
            {property: 'strategy',
             render: datum => <Button label="Remove"
             onClick = {(e) =>
                        fetch(APIURL + "/" + datum.id,
                              {method: "delete"}).then(
                                  resp => getAPIs()).then(
                                      apis => this.setState({apis}))
                       }
             />},
            {property: 'rate_limit', render:
             datum =>
             <Button label="Edit"
             onClick = {
                 (e) =>
                     fetch(APIURL + "/" + datum.id).then(
                         editing => this.setState({editing})
                     ).then(console.log(this.state)).then(
                         this.setState({url: APIURL + "/" + datum.id}))

             }
             />}
        ]}
        getAPIs().then(apis => this.setState({apis}))
    }

    render() {
        let editcomponent;
        if (this.state.editing) {
            editcomponent = <Box> <NewApiForm hostname={this.state.editing.hostname}
            frontend_prefix={this.state.editing.frontend_prefix}
            backend_prefix={this.state.editing.backend_prefix}
            auth_strategy={this.state.editing.auth_strategy}
            editurl={this.state.url}  /> </Box>;
        }
        else {
            editcomponent = <Box />;
        }
        return (
            <Box>
                <DataTable columns={this.state.columns} data={this.state.apis} />
                {editcomponent}
            </Box>
        )}
}

