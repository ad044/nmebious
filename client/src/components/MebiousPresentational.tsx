import React from "react";

import "../static/css/mebious.css";

const MebiousPresentational: React.FC<any> = (props) => {
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
