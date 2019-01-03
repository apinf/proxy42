import { combineReducers } from 'redux';
import apisReducer from './apis';

const cache = {
  apis: apisReducer,
};

export default combineReducers(cache);
