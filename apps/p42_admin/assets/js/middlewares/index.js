import thunkMiddleware from 'redux-thunk';
import dataloader from './dataloader';

const middlewares = [
  thunkMiddleware,
  dataloader,
];

export default middlewares;
