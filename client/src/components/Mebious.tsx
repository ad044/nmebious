import React, { useEffect, useState } from "react";
import MebiousPresentational from "./MebiousPresentational";
import { stylizeImg, corruptText, stylizeText } from "../utils/mebiousUtils";
import Topbar from "./Topbar";
import axios from "axios";

const Mebious: React.FC = () => {
  const [imageState, setImageState] = useState<Array<string>>([]);
  const [textState, setTextState] = useState<Array<string>>([]);

  const [textPresentationalState, setTextPresentationalState] = useState<
    Array<JSX.Element>
  >();
  const [imagePresentationalState, setImagePresentationalState] = useState<
    Array<JSX.Element>
  >();

  const getDifference = (
    arr1: Array<string>,
    arr2: Array<string>
  ): Array<string> => {
    return arr1.filter((x) => !arr2.includes(x));
  };

  const replaceItemsAtBeginning = <T extends {}>(
    arr1: Array<T>,
    arr2: Array<T>
  ): Array<T> => {
    return arr2.concat(arr1.slice(0, arr1.length - arr2.length));
  };

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

  useEffect(() => {
    const interval = setInterval(async () => {
      const textData = await fetchTextData();
      const imgData = await fetchImageData();

      if (textPresentationalState) {
        const textDiff = getDifference(textData, textState);
        const formattedTextDiff = textDiff.map((text: string) => (
          // using the text value as key itself because when replacing the items from the array indices get messy
          // this is fine since we have a no-duplicates rule.
          <h1 key={text} style={stylizeText()}>
            {corruptText(text)}
          </h1>
        ));

        // if the len of entries is < 10 no point in replacing items at the beginning of the array
        // just concat the diff with the current state
        if (textPresentationalState.length < 10) {
          setTextPresentationalState(
            formattedTextDiff.concat(textPresentationalState)
          );
          setTextState(textDiff.concat(textState));
        } else {
          setTextPresentationalState(
            replaceItemsAtBeginning(textPresentationalState, formattedTextDiff)
          );

          setTextState(replaceItemsAtBeginning(textState, textDiff));
        }
      }

      if (imagePresentationalState) {
        const imgDiff = getDifference(imgData, imageState);
        const formattedImgDiff = imgDiff.map((imgPath: string) => (
          // same as above, using image paths as keys
          <img key={imgPath} src={imgPath} alt="img" style={stylizeImg()}></img>
        ));

        if (imagePresentationalState.length < 10) {
          setImagePresentationalState(
            formattedImgDiff.concat(imagePresentationalState)
          );
          setImageState(imgDiff.concat(imageState));
        } else {
          setImagePresentationalState(
            replaceItemsAtBeginning(imagePresentationalState, formattedImgDiff)
          );

          setImageState(replaceItemsAtBeginning(imageState, imgDiff));
        }
      }
    }, 3000);

    return () => {
      clearInterval(interval);
    };
  });

  useEffect(() => {
    const initialSync = async () => {
      const imgFetch = await fetchImageData();
      const textFetch = await fetchTextData();

      setTextState(textFetch);
      setTextPresentationalState(
        textFetch.map((text: string) => (
          <h1 key={text} style={stylizeText()}>
            {corruptText(text)}
          </h1>
        ))
      );

      setImageState(imgFetch);
      setImagePresentationalState(
        imgFetch.map((imgPath: string) => (
          <img key={imgPath} src={imgPath} alt="img" style={stylizeImg()}></img>
        ))
      );
    };

    initialSync();
  }, []);

  // call fetch functions once on load
  return (
    <React.Fragment>
      <Topbar />
      <MebiousPresentational
        textPresentationalState={textPresentationalState!}
        imagePresentationalState={imagePresentationalState!}
      />
    </React.Fragment>
  );
};

export default Mebious;
