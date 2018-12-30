import { applyMiddleware, createStore, compose } from 'redux'
import thunkMiddleware from 'redux-thunk'
import rootReducer from './reducers'

const middlewares = [thunkMiddleware];
const middlewareEnhancer = applyMiddleware(...middlewares);

const enhancers = [middlewareEnhancer];
const composedEnhancers = compose(...enhancers);

const preloadedState = undefined;
const store = createStore(rootReducer, preloadedState, composedEnhancers);

export default store;
