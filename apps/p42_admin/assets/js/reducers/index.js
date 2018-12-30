import { combineReducers } from 'redux';
import { reducer as formReducer } from 'redux-form'

const reducers = {
    form: formReducer
};

const rootReducer = combineReducers(reducers);

export default rootReducer;
