import React from 'react'
import { MapContainer } from 'containers'
import { Header } from 'components'
import { BrowserRouter as Router, Route } from 'react-router-dom'
import styled from 'styled-components'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

const Container = styled.div`text-align: center;`

function Routes() {
  return (
    <Router>
      <MuiThemeProvider>
          <Container>
            <Header />
            <Route path="/" component={MapContainer} />
          </Container>
      </MuiThemeProvider>
    </Router>
  )
}

export default Routes
