import {
  validateImageMime,
  formatAndSaveImage,
  sha1Hash,
  isImageDuplicate,
} from "../utils/imageUtils";
import { Request, Response } from "express";
import { getRepository } from "typeorm";
import { getDate } from "../utils/generalUtils";
import Image from "../entities/Image";
import { isSpam } from "../utils/postUtils";

const getImages = async (req: Request, res: Response) => {
  return res.send(
    await getRepository(Image)
      .createQueryBuilder("image")
      .select(["image.fileName"])
      .limit(10)
      .orderBy("id", "DESC")
      .getMany()
  );
};

const postImage = async (req: Request, res: Response) => {
  const [filePath, fileName] = [req.file.path, req.file.filename];
  const ip =
    ((req.headers["x-forwarded-for"] as string) || "").split(",")[0] ||
    req.connection.remoteAddress!;
  const stamp = getDate();
  const spawn = Date.now();

  const spam = await isSpam(Image, ip);
  if (spam) return res.send({ reason: "spam", status: 400 });

  const imageFormat = validateImageMime(filePath);
  if (!imageFormat) return res.send({ reason: "invalid", status: 400 });

  const hash: string = await sha1Hash(filePath);

  // check if that hash exists in db
  const isDuplicate: Array<Image> | [] | undefined = await isImageDuplicate(
    hash
  );

  if (isDuplicate.length != 0)
    return res.send({ reason: "duplicate", status: 400 });

  const outputDest = __dirname + "/../uploads/" + fileName + imageFormat;
  formatAndSaveImage(filePath, outputDest);

  // save image to db
  const image = getRepository(Image).create({
    fileName: fileName + imageFormat,
    url: "",
    stamp: stamp,
    spawn: spawn,
    ip: ip,
    hash: hash,
  });

  getRepository(Image)
    .manager.save(image)
    .catch((err) => {
      console.log(err);
      return res.send({ reason: "error", status: 400 });
    });

  res.send(200);
};

export { postImage, getImages };
