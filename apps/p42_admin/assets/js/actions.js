import { SERVER_BASE_URL, ELASTIC_SEARCH_URL } from './constants'

export const loadAPI = (id) => ({
    type: 'LOAD_API',
    data: { id }
});

export const saveAPI = (_api) => {
    const api = {..._api, loaded: null, pending: null, error: null}
    return (dispatch) => {
        dispatch({
            type: 'SAVE_API_PENDING',
        });
        const method = api.id ? "PATCH" : "POST";
        const url = `${SERVER_BASE_URL}/apis/${api.id || ''}`;
        console.log(api);
        fetch(url ,
              {method,
               headers: {
                   "Content-Type": "application/json",
                   "Accept": "application/json"
               },
               body: JSON.stringify(api)
              }
             )
            .then(res => {
                if(res.ok) { res.json(); }
                else { throw Error(res.statusText); }
            })
            .then((data) => dispatch({
                type: 'SAVE_API_SUCCESS',
                data
            }))
            .catch((err) => dispatch({
                type: "SAVE_API_FAILED",
                err
            }))
    }
}

// export const loadChart = (id, timerange) => {
//     return dispatch => {
//         dispatch({
//             type: "LOAD_CHART_PENDING",
//         });
//         const url = `${ELASTIC_SEARCH_URL}/proxy42/_count`
//         fetch(url,
//               {method: "GET",
//                headers:
//                {"Content-Type": "application/json",
//                 "Accept": "application/json"
//                }
//               }
//              )
//             .then(res => {
//                 if(res.ok) {res.json(); }
//                 else {throw Error(res.statusText)}
//             }).then((data) => dispatch({
//                 type: "LOAD_CHART_SUCCESS",
//                 data
//             })).catch((err) => dispatch({
//                 type: "LOAD_CHART_FAILED",
//                 err
//             }))
//     }
// }
