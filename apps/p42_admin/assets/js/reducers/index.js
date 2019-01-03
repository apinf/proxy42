import { combineReducers } from 'redux';
import { reducer as formReducer } from 'redux-form';
import cacheReducer from './cache';

const reducers = {
  form: formReducer,
  cache: cacheReducer,
};

const rootReducer = combineReducers(reducers);

export default rootReducer;
