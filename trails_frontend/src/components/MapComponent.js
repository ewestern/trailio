
import React from 'react';
import PropTypes from 'prop-types'
import Paper from 'material-ui/Paper';
import { Map, TileLayer, Marker, Popup } from 'react-leaflet';


class MapComponent extends React.Component {
  static propTypes = {
    viewport: PropTypes.object.isRequired,
    onViewportChanged: PropTypes.func.isRequired
  }
  render() {
    var center, zoom;
    ({center,zoom} = this.props.viewport);
    return (
      <Paper style={{height: '400px', width: '600px'}} zDepth={2} >
        <Map style={{height: '400px', width: '600px'}} center={center} zoom={zoom} onViewportChanged={this.props.onViewportChanged}>
          <TileLayer
            attribution='&copy <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
            //url="https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZXdlc3Rlcm4iLCJhIjoiY2lmY2Z5eWNsM3Y2OHN4bTdndmJha29kZCJ9.8hIQ8iTAmMZD__3uHytwvw"
            url="https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZXdlc3Rlcm4iLCJhIjoiY2lmY2Z5eWNsM3Y2OHN4bTdndmJha29kZCJ9.8hIQ8iTAmMZD__3uHytwvw"
          />
        </Map>
      </Paper>
    )
  }
}

export default MapComponent;


