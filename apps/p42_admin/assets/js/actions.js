import { SERVER_BASE_URL } from './constants'

export const loadAPI = (id) => ({
  type: 'LOAD_API',
  data: { id }
});

export const saveAPI = (api) => {
    return (state, dispatch) => {
        dispatch({
            type: 'SAVE_API_PENDING',
        });
        const method = api.id ? "PATCH" : "POST";
        const url = `${SERVER_BASE_URL}/apis/${api.id || ''}`;
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
