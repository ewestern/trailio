
import React, {PropTypes} from 'react';
import { Map, TileLayer, Marker, Popup } from 'react-leaflet';

var MapComponent = React.createClass({
  getInitialState() {
    return {
      position: [36.01, -119],
      zoom: 13
    };
  },
  render() {
    return (
      <div >
        <Map style={{height: '400px', width: '600px'}} center={this.state.position} zoom={this.state.zoom}>
          <TileLayer
            attribution='&copy <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
            url="https://api.mapbox.com/styles/v1/mapbox/outdoors-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZXdlc3Rlcm4iLCJhIjoiY2lmY2Z5eWNsM3Y2OHN4bTdndmJha29kZCJ9.8hIQ8iTAmMZD__3uHytwvw"
          />
        </Map>
      </div>
    )
  }
});

export default MapComponent;


