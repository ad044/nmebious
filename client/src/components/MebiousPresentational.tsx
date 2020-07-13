import React from "react";

import "../static/css/mebious.css";

type MebiousState = {
  imagePresentationalState: Array<JSX.Element>;
  textPresentationalState: Array<JSX.Element>;
};

const MebiousPresentational: React.FC<MebiousState> = (props) => {
  return (
    <React.Fragment>
      <div id="posts">
        <div id="images">{props.imagePresentationalState}</div>
        <div id="text">{props.textPresentationalState}</div>
      </div>
    </React.Fragment>
  );
};

export default MebiousPresentational;
