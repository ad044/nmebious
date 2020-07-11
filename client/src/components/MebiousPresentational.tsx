import React from "react";

import "../static/css/mebious.css";
import { stylizeImg, corruptText } from "../utils/mebiousUtils";

const MebiousPresentational: React.FC<any> = (props) => {
  return (
    <React.Fragment>
      <div id="topbar">
        <form method="post" action="/api/text">
          <div id="upload_text">
            <input name="text" />
            <button id="text_button">in_txt</button>
          </div>
        </form>
        <form method="post" action="/api/urls">
          <div id="upload_url">
            <input name="imgUrl" />
            <button id="imgUrl">in_url</button>
          </div>
        </form>
        <form method="post" action="/api/images" encType="multipart/form-data">
          <div id="upload_img">
            <input name="img" type="file" />
            <button id="img_button">in_img</button>
          </div>
        </form>
      </div>

      <div id="posts">
        <div id="images">
          {props.imageState.map((imgPath: string, idx: number) => (
            <img key={idx} src={imgPath} alt="img" style={stylizeImg()}></img>
          ))}
        </div>
        <div id="text">
          {props.textState.map((text: string, idx: number) => (
            <h1>{corruptText(text)}</h1>
          ))}
        </div>
      </div>
    </React.Fragment>
  );
};

export default MebiousPresentational;
