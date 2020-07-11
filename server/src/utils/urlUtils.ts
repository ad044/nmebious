import fs from "fs";
import axios from "axios";
import dataUriToBuffer from "data-uri-to-buffer";

const isDataURI = (url: string) => {
  const uriRegex: RegExp = /^\s*data:([a-z]+\/[a-z]+(;[a-z\-]+\=[a-z\-]+)?)?(;base64)?,[a-z0-9\!\$\&\'\,\(\)\*\+\,\;\=\-\.\_\~\:\@\/\?\%\s]*\s*$/i;
  return url.match(uriRegex);
};

const downloadFromDataURI = (dataURI: string, filePath: string) => {
  // save buffer to file
  const decoded = dataUriToBuffer(dataURI);
  fs.createWriteStream(filePath).write(decoded);

  // separate out the mime component
  let mimeString = dataURI.split(",")[0].split(":")[1].split(";")[0];
  if (mimeString === "image/jpeg") return ".jpg";
  if (mimeString === "image/png") return ".png";
  return false;
};

const downloadImage = (url: string, filePath: string) => {
  return (
    axios
      .get(url, {
        method: "GET",
        responseType: "stream",
      })
      .then((response) => {
        const imageFormat: string = response.headers["content-type"];
        if (imageFormat === "image/png" || imageFormat === "image/jpeg") {
          response.data.pipe(fs.createWriteStream(filePath));
          if (imageFormat === "image/jpeg") return ".jpg";
          if (imageFormat === "image/png") return ".png";
          return false;
        }
      })
      // if the provided url wasn't a regular one try to check if its a base64 data uri
      .catch(() => {
        if (!isDataURI(url)) return false;
        return downloadFromDataURI(url, filePath);
      })
  );
};

export { downloadImage };
