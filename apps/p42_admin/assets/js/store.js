import { applyMiddleware, createStore, compose } from 'redux'
import rootReducer from './reducers'
import middlewares from './middlewares';

const middlewareEnhancer = applyMiddleware(...middlewares);

const enhancers = [middlewareEnhancer];
const composeEnhancers =
	typeof window === 'object' &&
	window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ ?   
	window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__({
		// Specify extension’s options like name, actionsBlacklist, actionsCreators, serialize...
	}) : compose;
const composedEnhancers = composeEnhancers(...enhancers);

const preloadedState = undefined;
const store = createStore(rootReducer, preloadedState, composedEnhancers);

export default store;
