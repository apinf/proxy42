import {SERVER_BASE_URL} from '../../constants';

const apiLoader = ({id}) =>
  fetch(`${SERVER_BASE_URL}/apis/${id}`)
    .then(res => {
      if(res.ok) { return res.json() }
      else { throw Error(res.statusText)}
    })

export default apiLoader;
