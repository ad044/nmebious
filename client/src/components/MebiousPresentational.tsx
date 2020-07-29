import React from "react";

import "../static/css/mebious.css";

type MebiousState = {
  imagePresentationalState: Array<JSX.Element>;
  textPresentationalState: Array<JSX.Element>;
  checked: boolean;
  setChecked: (checked: boolean) => void;
};

const MebiousPresentational: React.FC<MebiousState> = (props) => {
  return (
    <React.Fragment>
      <div id="posts">
        <div id="images">{props.imagePresentationalState}</div>
        <div id="text">{props.textPresentationalState}</div>
      </div>
      <div id="checkbox_container">
        <input
          id="checkbox"
          type="checkbox"
          checked={props.checked}
          onChange={() => props.setChecked(!props.checked)}
        />
        <h2>disable automatic updates</h2>
      </div>
    </React.Fragment>
  );
};

export default MebiousPresentational;
