import { getRepository } from "typeorm";
import Text from "../entities/Text";

const isAsciiOnly = (text: string) => {
  return !/[^\x20-\x7e]/.test(text);
};

const isTextDuplicate = async (text: string) => {
  return await getRepository(
    Text
  ).manager.query(
    "SELECT * FROM text WHERE text=$1 ORDER BY id DESC LIMIT 10",
    [text]
  );
};

export { isAsciiOnly, isTextDuplicate };
