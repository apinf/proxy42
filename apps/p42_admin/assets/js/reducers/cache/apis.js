const apisReducer = (apis = {}, action) => {
  let api = {...action.data}
  switch(action.type) {
    case 'LOAD_API_PENDING':
      return {...apis,
        [api.id]: {...api,
          loaded: false, error: false, pending: true}
      };
    case 'LOAD_API_SUCCESS':
      return {...apis,
        [api.id]: {...api,
          loaded: true, error: false, pending: false}
      };
    case 'LOAD_API_FAILED':
      return {...apis,
        [api.id]: {...api,
          loaded: true, error: true, pending: false}
      };
    default:
      return apis
  }
}

export default apisReducer;
