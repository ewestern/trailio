import React from 'react'
import PropTypes from 'prop-types'

import {List, ListItem} from 'material-ui/List';
import styled from 'styled-components'

import { MapComponent } from 'components'
import { createStructuredSelector, createSelector } from 'reselect'

import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import * as MapActions from 'actions/map'


const RowContainer = styled.div`display: flex; flex-direction: row;`
class MapContainer extends React.Component {
  static propTypes = {
    viewport: PropTypes.object.isRequired,
    segments: PropTypes.object.isRequired,
    selected: PropTypes.object.isRequired
  }
  render() {
    var that = this;
    return (
      <RowContainer>
        <MapComponent 
          style={{flex: 'auto'}}
          viewport={this.props.viewport}
          segments={this.props.segments}
          onViewportChanged={this.props.changeViewport}
          onSegmentClick={this.props.selectSegment}
          selected={this.props.selected}
        />
        <List style={{flex: 'auto'}}>
          {this.props.selected.toArray().map(function(s) {
              console.log("SEL", s);
              return (
                  <ListItem key={s.osmId.toString()} primaryText={"foo"} />
              )
          })}
        </List>
      </RowContainer>
    )
  }
}

// Connect Recact to Redux

const mapStateToProps = createStructuredSelector({
  viewport: createSelector(
    (state) => state.viewportState.viewport,
    (centerState) => centerState
  ),
  segments: createSelector(
    (state) => state.viewportState.segments,
    (cs) => cs
  ),
  selected: createSelector(
    (state) => state.selectedSegments,
    (cs) => cs
  )
})

function mapDispatchToProps(dispatch) {
  return bindActionCreators(MapActions, dispatch)
}

export default connect(mapStateToProps, mapDispatchToProps)(MapContainer)
