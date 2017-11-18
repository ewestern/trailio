import { combineReducers } from 'redux'
import {viewportState, selectedSegments} from './map'

const rootReducer = combineReducers({
  viewportState, selectedSegments
})

export default rootReducer
