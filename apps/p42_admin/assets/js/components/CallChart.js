import React, { Component } from 'react';
import { connect } from 'react-redux';

// import { Link } from "react-router-dom";
// import { loadChart } from "../actions";

import { Box, Stack, Text, Chart, calcs } from 'grommet'

import { Line as LineChart } from 'react-chartjs';

const buildURLQuery = obj =>
      Object.entries(obj)
      .filter(pair => pair[1])
      .map(pair => pair.map(encodeURIComponent).join('='))
      .join('&');

class CallChart extends Component {
    componentDidMount() {
        const query_params = {
            api_id: this.props.api_id,
            interval: this.props.interval,
        }
        let url = `/api/stats?${buildURLQuery(query_params)}`
        fetch(url,
              {method: "GET",
               headers:
               {"Content-Type": "application/json",
                "Accept": "application/json"
               },
              }
             ).then(res => {
                 if(res.ok) { return res.json();}
                 else {throw Error(res.statusText)};
             }).then(
                 (data) => this.setState(
                     this.formatdata(data)))
            .catch(err => console.err(err))
    }

    formatdata({calls_over_time: calls}) {
        const labels = Object.keys(calls).map(k => new Date(+k))
              .map(d => d.toLocaleDateString())
        const values = Object.keys(calls).map(k => calls[k])
        const datasets = [{data: values}]
        const chartData = {labels, datasets}
        console.log(chartData)
        return chartData
    }

    // getInitialState() {
    //     return {labels: [], datasets: []}
    // }

    render() {
    const props = {
        width: 600,
        height: 250,
        options: {},
        data: this.state,
    }
        return this.state && <LineChart {...props}/>
    }
}
export default CallChart;
