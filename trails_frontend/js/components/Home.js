import React, {Component} from 'react';
import {connect} from 'react-redux';
import {bindActionCreators} from 'redux';
import * as HomeActions from '../actions/HomeActions';
import MapComponent from './MapComponent.js';
//import styles from '../../css/app.css';


class Home extends Component {
  render() {
    const {title, dispatch} = this.props;
    const actions = bindActionCreators(HomeActions, dispatch);
    return (
        <MapComponent />
    );
  }
}

export default connect(state => state.Sample)(Home)
