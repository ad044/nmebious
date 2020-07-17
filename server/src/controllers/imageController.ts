import {
  validateImageMime,
  tryFormatAndSaveImage,
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

const getImagesTillN = async (req: Request, res: Response, n: number) => {
  return res.send(
    await getRepository(Image)
      .createQueryBuilder("image")
      .select(["image.fileName"])
      .limit(n)
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
  if (spam) return res.status(400).send({ reason: "spam" });

  const imageFormat = validateImageMime(filePath);
  if (!imageFormat) return res.status(400).send({ reason: "invalid" });

  const hash: string = await sha1Hash(filePath);

  // check if that hash exists in db
  const isDuplicate: Array<Image> | [] | undefined = await isImageDuplicate(
    hash
  );

  if (isDuplicate.length != 0)
    return res.status(400).send({ reason: "duplicate" });

  const outputDest = __dirname + "/../uploads/" + fileName + imageFormat;
  tryFormatAndSaveImage(filePath, outputDest)
    .then(() => {
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
          return res.status(400).send({ reason: "error" });
        });

      res.send(200);
    })
    .catch(() => {
      res.status(400).send({ reason: "error" });
    });
};

export { postImage, getImages, getImagesTillN };
