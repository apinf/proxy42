import apiLoader from './apiLoader';

// Map of type: loaderfun
const resourceLoaders = {
  'API': apiLoader
}

const dataloader = store => next => action => {
  // Pass non load actions through to next middleware
  if(!action.type.startsWith('LOAD_')) { return next(action) }

  // We intercept all load actions
  // The resource type to be loaded is in action type, after LOAD_
  const name = action.type
  const resourceType = name.substring(5)
  const resourceLoader = resourceLoaders[resourceType];
  // We fire a pending action first. Can be used to show spinners
  next({type: `${name}_PENDING`});
  // Now do async loading
  resourceLoader(action.data)
    .then((data) =>
      next({ type: `${name}_SUCCESS`, data }))
    .catch((err) =>
      next({ type: `${name}_FAILED`, err }))
}

export default dataloader;
