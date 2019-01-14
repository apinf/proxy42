import React, {Component} from 'react';
import { DataTable, Box, Text, Button, Grommet } from 'grommet';
import { connect } from "react-redux";
import { NewApiForm } from "../apis"

import { withRouter } from "react-router-dom";

let APIURL = "http://84.20.150.158:4001/apis"
let getAPIs = () => fetch(`${APIURL}`)
  .then(res => res.json())

@withRouter
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
              fetch(APIURL + "/" + datum.id, {method: "delete"})
                .then( resp => getAPIs())
                .then( apis => this.setState({apis}))
          }
        />},
      {property: 'rate_limit', render:
        datum => <div>
          <Button label="View" onClick={() => this.props.history.push(`/apis/${datum.id}`)} />
        <Button label="Edit"
          onClick = {() => this.props.history.push(`/apis/${datum.id}/edit`) }
        /></div>}
    ]}
    getAPIs().then(apis => this.setState({apis}))
  }

  render() {
    return (
      <Box>
        <DataTable columns={this.state.columns} data={this.state.apis} />
      </Box>
    )}
}

