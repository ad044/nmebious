import { Request, Response } from "express";
import { getRepository } from "typeorm";
import { getDate } from "../utils/generalUtils";
import { isSpam } from "../utils/postUtils";
import Text from "../entities/Text";
import { isTextDuplicate, isAsciiOnly } from "../utils/textUtils";

const postText = async (req: Request, res: Response) => {
  const submittedText: string = req.body.text;
  const ip =
    ((req.headers["x-forwarded-for"] as string) || "").split(",")[0] ||
    req.connection.remoteAddress!;
  const stamp = getDate();
  const spawn = Date.now();

  const spam = await isSpam(Text, ip);
  if (spam) return res.send({ reason: "spam", status: 400 });

  const isDuplicate: Array<Text> | [] | undefined = await isTextDuplicate(
    submittedText
  );

  if (isDuplicate.length != 0)
    return res.send({ reason: "duplicate", status: 400 });

  if (!isAsciiOnly(submittedText))
    return res.send({ reason: "nonascii", status: 400 });

  // save text to db
  const text = getRepository(Text).create({
    ip: ip,
    stamp: stamp,
    text: submittedText,
    spawn: spawn,
    ...req.body,
  });

  getRepository(Text)
    .manager.save(text)
    .catch((err) => {
      console.log(err);
      return res.send({ reason: "error", status: 400 });
    });

  res.send(200);
};

const getText = async (req: Request, res: Response) => {
  return res.send(
    await getRepository(Text)
      .createQueryBuilder("text")
      .select(["text.text"])
      .limit(10)
      .orderBy("id", "DESC")
      .getMany()
  );
};

export { postText, getText };
