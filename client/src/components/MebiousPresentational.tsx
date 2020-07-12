import React from "react";

import "../static/css/mebious.css";
import { stylizeImg, corruptText, stylizeText } from "../utils/mebiousUtils";
import Topbar from "./Topbar";

const MebiousPresentational: React.FC<any> = (props) => {
  return (
    <React.Fragment>
      <Topbar />
      <div id="posts">
        <div id="images">
          {props.imageState.map((imgPath: string, idx: number) => (
            <img key={idx} src={imgPath} alt="img" style={stylizeImg()}></img>
          ))}
        </div>
        <div id="text">
          {props.textState.map((text: string, idx: number) => (
            <h1 key={idx} style={stylizeText()}>
              {corruptText(text)}
            </h1>
          ))}
        </div>
      </div>
    </React.Fragment>
  );
};

export default MebiousPresentational;
