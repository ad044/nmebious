import React from "react";

type TopbarState = {
  textInput: string;
  urlInput: string;
  errorState: string;

  textBoxError: boolean;
  urlBoxError: boolean;
  imageBoxError: boolean;

  fileSelectHandler: (e: React.ChangeEvent) => void;
  submitTo: (
    e: React.MouseEvent<HTMLElement>,
    to: string,
    data: object
  ) => void;
  submitImage: (e: React.MouseEvent<HTMLElement, MouseEvent>) => void;

  setTextInput: (text: string) => void;
  setUrlInput: (url: string) => void;
};

const TopbarPresentational: React.FC<TopbarState> = (props) => {
  return (
    <div id="topbar">
      <form>
        <div className="uploads">
          <input
            style={
              props.textBoxError
                ? { color: "#FF0000", border: "1px solid #FF0000" }
                : {}
            }
            type="text"
            value={props.textInput}
            onChange={(e) => props.setTextInput(e.target.value)}
          />
          <button
            style={
              props.textBoxError
                ? { border: "1px solid #FF0000", color: "#FF0000" }
                : {}
            }
            id="text"
            onClick={(e: React.MouseEvent<HTMLElement>) =>
              props.submitTo(e, "/api/text", { text: props.textInput })
            }
          >
            in_txt
          </button>
        </div>
      </form>
      <form>
        <div className="uploads">
          <input
            style={
              props.urlBoxError
                ? { color: "#FF0000", border: "1px solid #FF0000" }
                : {}
            }
            type="text"
            value={props.urlInput}
            onChange={(e) => props.setUrlInput(e.target.value)}
          />
          <button
            style={
              props.urlBoxError
                ? { border: "1px solid #FF0000", color: "#FF0000" }
                : {}
            }
            id="url"
            onClick={(e: React.MouseEvent<HTMLElement>) =>
              props.submitTo(e, "/api/urls", { imgUrl: props.urlInput })
            }
          >
            in_url
          </button>
        </div>
      </form>
      <form>
        <div className="uploads">
          <input
            style={
              props.imageBoxError
                ? { border: "1px solid #FF0000", color: "#FF0000" }
                : {}
            }
            type="file"
            onChange={(e: React.ChangeEvent) =>
              props.fileSelectHandler(e)
            }
          />
          <button
            style={
              props.imageBoxError
                ? { border: "1px solid #FF0000", color: "#FF0000" }
                : {}
            }
            id="img_button"
            onClick={(e: React.MouseEvent<HTMLElement>) => props.submitImage(e)}
          >
            in_img
          </button>
        </div>
      </form>
      <h1 id="error_text">{props.errorState}</h1>
    </div>
  );
};

export default TopbarPresentational;
