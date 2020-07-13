import React, { useState } from "react";
import axios from "axios";
import TopbarPresentational from "./TopbarPresentational";

const Topbar: React.FC = () => {
  const [textInput, setTextInput] = useState("");
  const [urlInput, setUrlInput] = useState("");
  const [imgInput, setImgInput] = useState<File>();

  const [errorState, setErrorState] = useState("");

  const [textBoxError, setTextBoxError] = useState(false);
  const [urlBoxError, setUrlBoxError] = useState(false);
  const [imageBoxError, setImageBoxError] = useState(false);

  const fileSelectHandler = (e: React.ChangeEvent) => {
    setImgInput((e as any).target.files[0]);
  };

  const resetErrors = () => {
    setTextBoxError(false);
    setUrlBoxError(false);
    setImageBoxError(false);
    setErrorState("");
  };

  // image submissions
  const submitImage = (e: React.MouseEvent<HTMLElement>) => {
    e.preventDefault();
    const imgData = new FormData();

    if (!imgInput) {
      setTextBoxError(false);
      setUrlBoxError(false);

      setErrorState("Empty.");
      setImageBoxError(true);
    } else {
      imgData.append("img", imgInput!, imgInput!.name);
      axios
        .post("/api/images", imgData, {
          headers: {
            "content-type": "multipart/form-data",
          },
        })
        .then(() => {
          resetErrors();
        })
        .catch((err) => {
          setImageBoxError(true);
          if (!err.response) return setErrorState("Error.");
          switch (err.response.data.reason) {
            case "empty":
              setErrorState("Empty.");
              break;
            case "spam":
              setErrorState("Spam.");
              break;
            case "invalid":
              setErrorState("Invalid.");
              break;
            case "duplicate":
              setErrorState("Duplicate.");
              break;
            case "error":
              setErrorState("Error.");
          }
        });
    }
  };

  // text/url submissions
  const submitTo = (
    e: React.MouseEvent<HTMLElement>,
    to: string,
    data: Object
  ) => {
    e.preventDefault();
    const eventTarget = e.currentTarget.id;
    axios
      .post(to, data)
      .then(() => {
        resetErrors();
      })
      .catch((err) => {
        if (!err.response) return setErrorState("Error.");
        switch (err.response.data.reason) {
          case "duplicate":
            setErrorState("Duplicate.");
            break;
          case "nonascii":
            setErrorState("Posts can't contain non-ascii characters.");
            break;
          case "spam":
            setErrorState("Spam.");
            break;
          case "invalid":
            setErrorState("Invalid.");
            break;
          case "error":
            setErrorState("Error.");
            break;
          case "empty":
            setErrorState("Empty.");
        }
        switch (eventTarget) {
          case "text":
            setUrlBoxError(false);
            setImageBoxError(false);

            setTextBoxError(true);
            break;
          case "url":
            setTextBoxError(false);
            setImageBoxError(false);

            setUrlBoxError(true);
        }
      });
  };

  return (
    <TopbarPresentational
      textInput={textInput}
      urlInput={urlInput}
      errorState={errorState}
      textBoxError={textBoxError}
      urlBoxError={urlBoxError}
      imageBoxError={imageBoxError}
      fileSelectHandler={fileSelectHandler}
      submitTo={submitTo}
      submitImage={submitImage}
      setTextInput={setTextInput}
      setUrlInput={setUrlInput}
    />
  );
};

export default Topbar;
