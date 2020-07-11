import {
  isImageDuplicate,
  formatAndSaveImage,
  sha1Hash,
} from "../utils/imageUtils";
import { downloadImage } from "../utils/urlUtils";
import { Request, Response } from "express";
import { getRepository } from "typeorm";
import { getDate } from "../utils/generalUtils";
import Image from "../entities/Image";
import path from "path";
import crypto from "crypto";
import { isSpam } from "../utils/postUtils";

const postUrl = async (req: Request, res: Response) => {
  const fileName = crypto.randomBytes(16).toString("hex");
  const filePath = path.join(__dirname + "/../temp/" + fileName);
  const ip =
    ((req.headers["x-forwarded-for"] as string) || "").split(",")[0] ||
    req.connection.remoteAddress!;
  const stamp = getDate();
  const spawn = Date.now();

  const spam = await isSpam(Image, ip);
  if (spam) return res.status(400).send({ reason: "spam" });

  // downloads the image from url to filePath and returns the image format
  const imageFormat = await downloadImage(req.body.imgUrl, filePath);
  if (!imageFormat) return res.status(400).send({ reason: "invalid" });

  // get hash from the image
  const hash: string = await sha1Hash(filePath);

  // check if that hash exists in db
  const isDuplicate: Array<Image> | [] | undefined = await isImageDuplicate(
    hash
  );

  if (isDuplicate.length != 0)
    return res.status(400).send({ reason: "duplicate" });

  const outputDest = __dirname + "/../uploads/" + fileName + imageFormat;
  formatAndSaveImage(filePath, outputDest);

  // save image to db
  const image = getRepository(Image).create({
    fileName: fileName + imageFormat,
    url: req.body.imgUrl,
    stamp: stamp,
    spawn: spawn,
    ip: ip,
    hash: hash,
  });

  getRepository(Image)
    .manager.save(image)
    .catch((err) => {
      console.log(err);
      return res.status(400).send({ reason: "error" });
    });

  res.send(200);
};

export { postUrl };
