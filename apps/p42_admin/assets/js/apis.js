import React, { Component } from 'react';

const JsonTable = require('ts-react-json-table');

fetch("http://httpbin.org/headers")
  .then(res => res.json())
  .then(res => console.log(res))

let APIURL = "http://84.20.150.158:4001/apis"
let getAPI = (id) => fetch(`${APIURL}/${id}`)
  .then(res => res.json())
let getAPIs = () => fetch(`${APIURL}`)
  .then(res => res.json())


export class NewApiForm extends Component {
  render() {
    return (
      <div>
      <form>
      </form>
      </div>
  )}
}

export class AllApisTable extends Component {
  
  constructor(props){
    super(props)
    getAPIs().then(apis => this.setState(apis))  
  }


  render() {
    let columns = ['id',
      {key: 'frontend_prefix', label: 'From'},
      {key: 'backend_prefix', label: 'To'},
      {key: 'auth_config.strategy', label: 'Auth Strategy'}];
    console.log(this.state)
    return (
      <div>
        <JsonTable rows = {this.state} columns ={columns}/>
      </div>
  )}
}

export class ViewApi extends Component {
  render() {
    return (
      <div>ViewApi</div>
  )}
}




