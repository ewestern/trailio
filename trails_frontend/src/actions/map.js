import { CHANGE_VIEWPORT, SELECT_SEGMENT } from 'constants/ActionTypes'
import { createAction } from 'redux-actions'
import Leaflet from 'leaflet'

function viewportToBounds(vp, size={x:600,y:400}) {
  var sizeP = Leaflet.point(size),
      half = sizeP.divideBy(2),
      point = Leaflet.CRS.EPSG4326.latLngToPoint(Leaflet.latLng(vp.center), vp.zoom),
      sw = Leaflet.CRS.EPSG4326.pointToLatLng(point.subtract(half), vp.zoom), 
      ne = Leaflet.CRS.EPSG4326.pointToLatLng(point.add(half), vp.zoom);
  return [sw, ne]; ///Leaflet.latLngBounds(sw, ne)
}
const getSegment = function(viewport) {
  var [sw, ne] = viewportToBounds(viewport),
      swa = [sw.lat, sw.lng],
      nea = [ne.lat, ne.lng];
  return new Promise((resolve, reject) => {
    window.getSegmentBoundsBySwByNe(swa, nea, 4326, resolve, reject);
  });
}
export const changeViewport = createAction(CHANGE_VIEWPORT, async viewport => {
  const result = await getSegment(viewport)
  return result;
})

export const selectSegment = createAction (SELECT_SEGMENT)
