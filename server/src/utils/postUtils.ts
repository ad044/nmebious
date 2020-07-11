import Ban from "../entities/Ban";
import { getRepository, EntitySchema } from "typeorm";
import { Request, Response } from "express";

const isBanned = async (req: Request, res: Response, next: Function) => {
  const ip =
    ((req.headers["x-forwarded-for"] as string) || "").split(",")[0] ||
    req.connection.remoteAddress!;

  const ban: Ban | undefined = await getRepository(Ban).findOne({
    where: {
      ip: ip,
    },
  });

  if (ban) return res.status(400).send({ reason: "banned" });
  next();
};

const isSpam = async (
  entity: string | Function | (new () => unknown) | EntitySchema<unknown>,
  ip: string
) => {
  const lastPost = await getRepository(entity)
    .createQueryBuilder("entity")
    .where({ ip: ip })
    .orderBy("id", "DESC")
    .limit(1)
    .getRawOne();

  // if the user has posted in the last 20 seconds, mark it as spam
  if (lastPost && Date.now() - parseInt(lastPost.entity_spawn) < 20000)
    return true;
  return false;
};

export { isBanned, isSpam };
