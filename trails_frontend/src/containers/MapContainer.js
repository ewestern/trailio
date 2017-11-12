import React from 'react'
import PropTypes from 'prop-types'
//import { Counter } from 'components'
import { MapComponent } from 'components'
import { createStructuredSelector, createSelector } from 'reselect'

import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
//import * as CounterActions from 'actions/counter'
import * as MapActions from 'actions/map'


class MapContainer extends React.Component {
  static propTypes = {
    //center: PropTypes.array.isRequired,
    viewport: PropTypes.object.isRequired,
  }
  //changeViewport(asd) {
    //console.log("cvp", asd)
  //}
  render() {
    console.log("AS", this.props);
    return (
      <MapComponent 
        viewport={this.props.viewport}
        onViewportChanged={this.props.changeViewport}
      />
    )
  }
}

// Connect Recact to Redux

const mapStateToProps = createStructuredSelector({
  viewport: createSelector(
    //(state) => state.viewport,
    function(s) { console.log("s", s); return s.viewport },
    //(centerState) => centerState
    function(s) { console.log("s2", s); return s }
  )

})

function mapDispatchToProps(dispatch) {
  return bindActionCreators(MapActions, dispatch)
}

export default connect(mapStateToProps, mapDispatchToProps)(MapContainer)
