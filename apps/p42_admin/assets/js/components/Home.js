import React, { Component } from 'react';
import { Link } from "react-router-dom";

class Home extends Component {
  render() {
    return(
      <section>
        Menu: (TODO: beautify)

        <ul>
          <li><Link to="/apis">List existing apis</Link></li>
          <li><Link to="/apis/new">Add new API</Link></li>
          <li><Link to="/apis/123/edit">edit</Link></li>
        </ul>
      </section>
    )
  }
}

export default Home;
