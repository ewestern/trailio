import { CHANGE_VIEWPORT } from 'constants/ActionTypes'
import { createAction } from 'redux-actions'
//import 'assets/api.js'
//const getSegmentProximityByPointByDistance = require("exports?getSegmentProximityByPointByDistance!assets/api.js")



//export const changeViewport = createAction(CHANGE_VIEWPORT)
//export const changeViewport = function(asd){
  //console.log("ASD", asd);
//}

function onSuccess(asd){
  console.log("SUCC", asd)
}
function onError(asd){
  console.log("ERR", asd)
}
const getSegment = function(viewport) {
  console.log("ASD", viewport, window.getSegmentProximityByPointByDistance);
  return new Promise((resolve, reject) => {
    window.getSegmentProximityByPointByDistance([-118,0, 36], 100, 4326, function(asd) { console.log('DDD', asd); resolve(asd)}, function(asd) { console.log('EEE', asd); reject(asd)})
  });
}
export const changeViewport = createAction(CHANGE_VIEWPORT, async foo => {
  console.log("REQ", foo);
  const result = await getSegment(foo)
  console.log("RES", result)
  return result;
})
//export const increment = createAction(INCREMENT_COUNTER)
//
//export const decrement = createAction(DECREMENT_COUNTER)

//export function incrementIfOdd() {
//  return (dispatch, getState) => {
//    const { counter } = getState()
//
//    if (counter % 2 === 0) {
//      return
//    }
//
//    dispatch(increment())
//  }
//}
