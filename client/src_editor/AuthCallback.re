open Utils;
let component = ReasonReact.statelessComponent("AuthCallback");

let make = (~token, _children) => {
  ...component,
  render: _self => "Authenticating..." |. str,
};
