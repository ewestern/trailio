import { handleAction, combineActions } from 'redux-actions'
import { CHANGE_VIEWPORT, SELECT_SEGMENT } from 'constants/ActionTypes'
import I from 'immutable'

const initialViewport = {
  center: [36.01, -119],
  zoom : 13
}

const initialViewportState = {
  viewport: initialViewport,
  segments: I.Map()
}

/// name of function is mapped to attribute in state
function take(state, action) {
  var map = action.payload.reduce(function(acc, v) {
      return acc[v[0]] = v[1], acc
  }, {})
  var payload = I.Map(map);
  return {
    viewport: state.viewport,
    segments: state.segments.merge(payload)
  }
}

function select(state, action) {
  return state.set(action.payload.osmId, action.payload);
} 
export const viewportState = handleAction(CHANGE_VIEWPORT, take, initialViewportState)
export const selectedSegments = handleAction(SELECT_SEGMENT, select, I.Map())
