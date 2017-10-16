const menu = (state = { hide: true }, action) => {
  console.log("menu reducer: " + action.type);
  switch (action.type) {
    case 'TOGGLE_MENU':
      return
        {
          hide: !state.hide
        };
    default:
      return state;
  }
}

export default menu

