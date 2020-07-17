import fs from "fs";
import gm from "gm";
import path from "path";
import crypto from "crypto";
import { getRepository } from "typeorm";
import { genRandomNumBetween } from "../utils/generalUtils";
import Image from "../entities/Image";

// waits for the image tempfile to appear in order for gm to manipulate it
const waitForFile = (filePath: string, timeout: number) => {
  return new Promise(function (resolve, reject) {
    const timer = setTimeout(function () {
      watcher.close();
      reject(new Error("File not found."));
    }, timeout);

    fs.access(filePath, fs.constants.R_OK, function (err) {
      if (!err) {
        clearTimeout(timer);
        watcher.close();
        resolve();
      }
    });

    const dir = path.dirname(filePath);
    const basename = path.basename(filePath);
    const watcher = fs.watch(dir, function (eventType, filename) {
      if (eventType === "rename" && filename === basename) {
        clearTimeout(timer);
        watcher.close();
        resolve();
      }
    });
  });
};

const validateImageMime = (filePath: string) => {
  const magicNumberInBody = fs.readFileSync(filePath).toString("hex", 0, 3);
  const magic = { jpg: "ffd8ff", png: "89504e" };
  if (magicNumberInBody == magic.jpg) {
    return ".jpg";
  } else if (magicNumberInBody == magic.png) {
    return ".png";
  } else {
    return false;
  }
};

const calculateAspectRatioFit = (
  srcWidth: number,
  srcHeight: number,
  maxWidth: number,
  maxHeight: number
) => {
  const ratio = Math.min(maxWidth / srcWidth, maxHeight / srcHeight);
  return { width: srcWidth * ratio, height: srcHeight * ratio };
};

const tryFormatAndSaveImage = (
  filePath: string,
  outputDest: string
): Promise<boolean> => {
  return new Promise((resolve, reject) => {
    // waitForFile didn't work quite well, so this interval checks if GM can access the image data
    // if it cant access it within 2 seconds it gives up
    const gmCheckInterval = setInterval(() => {
      try {
        gm(filePath).identify((err: Error, data: gm.ImageInfo) => {
          if (!err) {
            try {
              const { width, height } = calculateAspectRatioFit(
                data["size"]["width"],
                data["size"]["width"],
                300,
                300
              );
              gm(filePath)
                .resize(width, height)
                .colorspace("gray")
                .colors(3)
                .modulate(50)
                .colorize(0, genRandomNumBetween(10, 35), 0)
                .write(outputDest, (err) => {
                  if (err) reject(false);
                  resolve(true);
                });
            } catch (err) {
              console.log(err);
            }
          }
        });
      } catch (err) {
        console.log(err);
      }
    }, 500);
    setTimeout(() => {
      clearInterval(gmCheckInterval);
      reject(false);
    }, 5000);
  });
};

const sha1Hash = (path: string) =>
  new Promise<string>((resolve, reject) => {
    waitForFile(path, 5000).then(() => {
      const hash = crypto.createHash("sha1");
      const rs = fs.createReadStream(path);
      rs.on("error", reject);
      rs.on("data", (chunk) => hash.update(chunk));
      rs.on("end", () => resolve(hash.digest("hex")));
    });
  });

const isImageDuplicate = async (hash: string) => {
  return await getRepository(
    Image
  ).manager.query(
    "SELECT * FROM image WHERE hash=$1 ORDER BY id DESC LIMIT 10",
    [hash]
  );
};

export {
  isImageDuplicate,
  validateImageMime,
  tryFormatAndSaveImage,
  waitForFile,
  sha1Hash,
};
