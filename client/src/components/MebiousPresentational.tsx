import React from "react";

import "../static/css/mebious.css";
import { stylizeImg, corruptText, stylizeText } from "../utils/mebiousUtils";

const MebiousPresentational: React.FC<any> = (props) => {
  return (
    <React.Fragment>
      <div id="posts">
        <div id="images">
          {props.imageState.map((imgPath: string, idx: number) => (
            <img key={idx} src={imgPath} alt="img" style={stylizeImg()}></img>
          ))}
        </div>
        <div id="text">{props.textPresentationalState}</div>
      </div>
    </React.Fragment>
  );
};

export default MebiousPresentational;
