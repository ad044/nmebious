import { CSSProperties } from "react";

const genRandomNumBetween = (min: number, max: number) => {
  return Math.random() * (max - min) + min;
};

const shuffle = (arr: Array<{ [key: string]: any }>) => {
  for (let i = arr.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [arr[i], arr[j]] = [arr[j], arr[i]];
  }
  return arr;
};

const randomFromArr = (arr: Array<string>) => {
  return arr[Math.floor(Math.random() * arr.length)];
};

const stylizeImg = (): CSSProperties => {
  return {
    position: "absolute",
    zIndex: Math.round(genRandomNumBetween(1, 10)),
    opacity: genRandomNumBetween(0.5, 1),
    left: `${genRandomNumBetween(0.1, 70)}%`,
    top: `${genRandomNumBetween(0.1, 70)}%`,
  };
};

const corrupt = (char: string) => {
  const corruptionsA = ["@", "à", "ã", "á"];
  const corruptionsE = ["è", "ë", "ê"];
  const corruptionsI = ["ï", "î", "1"];
  const corruptionsO = ["ø", "ò", "ô"];
  const corruptionsU = ["ü", "ù"];
  const corruptionsY = ["ÿ"];
  const corruptionsS = ["$"];

  // 50% chance of going into this func
  if (Math.random() < 0.5) {
    switch (char) {
      // 20% chance of corrupting
      case "a":
        if (Math.random() < 0.7) return randomFromArr(corruptionsA);
      case "e":
        if (Math.random() < 0.7) return randomFromArr(corruptionsE);
      case "i":
        if (Math.random() < 0.7) return randomFromArr(corruptionsI);
      case "o":
        if (Math.random() < 0.7) return randomFromArr(corruptionsO);
      case "u":
        if (Math.random() < 0.7) return randomFromArr(corruptionsU);
      case "y":
        if (Math.random() < 0.7) return randomFromArr(corruptionsY);
      case "s":
        if (Math.random() < 0.7) return randomFromArr(corruptionsS);
    }
  }
  return char;
};

const corruptText = (str: string) => {
  const corruptArr: Array<string> = str
    .split("")
    .map((char: string) => corrupt(char.toLocaleLowerCase()));

  return corruptArr.join("");
};

export { corruptText, stylizeImg };
