import React, { Component } from 'react';
import './App.css';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import AppBar from 'material-ui/AppBar';
import {connect} from 'react-redux'

const App = ({hideMenu, toggleMenu}) => {
//class App extends Component {
//  render() {
    return (
      <MuiThemeProvider>
	<AppBar
          title = { "Kolhydraträknaren" }
          onLeftIconButtonTouchTap = { toggleMenu() }
        />
      </MuiThemeProvider>
    );
// }
}

const mapStateToProps = function(state) {
  return {
    hideMenu : state.hide
  }
}

const mapDispatchToProps = function(dispatch) {
  return { 
    toggleMenu : () => dispatch({
      type : 'TOGGLE_MENU'
    })
  }
}

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(App)
