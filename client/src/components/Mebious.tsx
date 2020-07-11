import React, { useEffect, useState } from "react";
import MebiousPresentational from "./MebiousPresentational";
import axios from "axios";

const Mebious: React.FC = () => {
  const [textInput, setTextInput] = useState("");

  const [imageState, setImageState] = useState<Array<string>>([]);
  const [textState, setTextState] = useState<Array<string>>([]);

  const submitText = (e: any, text: string) => {
    e.preventDefault();
    axios
      .post("/api/text", { text: text })
      .then((res) => {
        console.log(res)
      })
      .catch(() => {
        console.log("not sex");
      });
  };

  useEffect(() => {
    const fetchImageData = () => {
      axios
        .get("/api/images", {
          headers: {
            "Access-Control-Allow-Origin": "*",
          },
        })
        .then((res) => {
          setImageState(
            res.data.map(
              (imageEntry: { fileName: string }) => imageEntry.fileName
            )
          );
        });
    };

    const fetchTextData = () => {
      axios
        .get("/api/text", {
          headers: {
            "Access-Control-Allow-Origin": "*",
          },
        })
        .then((res) => {
          setTextState(
            res.data.map((textEntry: { text: string }) => textEntry.text)
          );
        });
    };

    fetchTextData();
    fetchImageData();
  }, []);

  return (
    <MebiousPresentational
      imageState={imageState}
      textState={textState}
      textInput={textInput}
      submitText={submitText}
      setTextInput={setTextInput}
    />
  );
};

export default Mebious;
