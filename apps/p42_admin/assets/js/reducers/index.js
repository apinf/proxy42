import { combineReducers } from 'redux';
import { reducer as formReducer } from 'redux-form';
import cacheReducer from './cache';
// import chartReducer from './chart';

const reducers = {
    form: formReducer,
    //chart: chartReducer,
    cache: cacheReducer,
};

const rootReducer = combineReducers(reducers);

export default rootReducer;
