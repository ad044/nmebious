import React, { useEffect, useState, ReactElement } from "react";
import MebiousPresentational from "./MebiousPresentational";
import { stylizeImg, corruptText, stylizeText } from "../utils/mebiousUtils";
import Topbar from "./Topbar";
import axios from "axios";

const Mebious: React.FC = () => {
  const [imageState, setImageState] = useState<Array<string>>([]);
  const [textState, setTextState] = useState<Array<string>>([]);

  const [textPresentationalState, setTextPresentationalState] = useState<
    Array<ReactElement>
  >();

  useEffect(() => {
    const fetchImageData = () => {
      return axios
        .get("/api/images", {
          headers: {
            "Access-Control-Allow-Origin": "*",
          },
        })
        .then((res) => {
          return res.data.map(
            (imageEntry: { fileName: string }) => imageEntry.fileName
          );
        });
    };

    const fetchTextData = () => {
      return axios
        .get("/api/text", {
          headers: {
            "Access-Control-Allow-Origin": "*",
          },
        })
        .then((res) => {
          return res.data.map((textEntry: { text: string }) => textEntry.text);
        });
    };

    const getDifference = (
      arr1: Array<string>,
      arr2: Array<string>
    ): Array<string> => {
      return arr1.filter((x) => !arr2.includes(x));
    };

    const replaceItemsAtEnd = (arr1: Array<string>, arr2: Array<string>) => {
      return arr1.slice(0, arr1.length - arr2.length).concat(arr2);
    };

    const interval = setInterval(async () => {
      const textData = await fetchTextData();
      const imgData = await fetchImageData();

      setImageState(imgData);
      setTextState(textData);

      //const imgDiff = getDifference(imgData, imageState);

      const textDiff = getDifference(textData, textState);
      const replaced = replaceItemsAtEnd(textState, textDiff);
      console.log(replaced)
      setTextPresentationalState(
        replaced.map((text: string, idx: number) => (
          <h1 key={idx} style={stylizeText()}>
            {corruptText(text)}
          </h1>
        ))
      );
    }, 3000);

    return () => {
      clearInterval(interval);
    };
  }, [textState, imageState]);

  // call fetch functions once on load
  return (
    <React.Fragment>
      <Topbar imageState={imageState} textState={textState} />
      <MebiousPresentational
        imageState={imageState}
        textPresentationalState={textPresentationalState}
      />
    </React.Fragment>
  );
};

export default Mebious;
