import React, { useEffect, useState } from "react";
import MebiousPresentational from "./MebiousPresentational";
import axios from "axios";

const Mebious: React.FC = () => {
  const [imageState, setImageState] = useState<Array<string>>([]);
  const [textState, setTextState] = useState<Array<string>>([]);

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
    <MebiousPresentational imageState={imageState} textState={textState} />
  );
};

export default Mebious;
