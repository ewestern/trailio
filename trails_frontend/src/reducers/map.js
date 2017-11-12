import { ZOOM_IN, ZOOM_OUT, MOVE_TO } from 'constants/ActionTypes'
import { handleAction } from 'redux-actions'
import { CHANGE_VIEWPORT } from 'constants/ActionTypes'

const initialViewport = {
  center: [36.01, -119],
  zoom : 13
}




export const viewport = handleAction(CHANGE_VIEWPORT, (state,action) => state, initialViewport)

//export function viewport(state=initialViewport, action) {
  //switch (action.type) {
    //case MOVE_TO:
      //return {
        //position: action.value,
        //zoom: state.zoom
      //} 
    //case ZOOM_IN:
      //return {
        //position: state.position,
        //zoom: state.zoom + 1
      //}
    //case ZOOM_OUT:
      //return {
        //position: state.position,
        //zoom: state.zoom - 1
      //}
    //default:
      //return state
  //}
//}


//export default function counter(state = initialState, action) {
  //switch (action.type) {
    //case INCREMENT_COUNTER:
      //return state + 1
    //case DECREMENT_COUNTER:
      //return state - 1
    //default:
      //return state
  //}
//}
