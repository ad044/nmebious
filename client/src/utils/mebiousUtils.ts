import { CSSProperties } from "react";

const genRandomNumBetween = (min: number, max: number) => {
  return Math.random() * (max - min) + min;
};

const randomFromArr = (arr: Array<string>) => {
  return arr[Math.floor(Math.random() * arr.length)];
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
        break;
      case "e":
        if (Math.random() < 0.7) return randomFromArr(corruptionsE);
        break;
      case "i":
        if (Math.random() < 0.7) return randomFromArr(corruptionsI);
        break;
      case "o":
        if (Math.random() < 0.7) return randomFromArr(corruptionsO);
        break;
      case "u":
        if (Math.random() < 0.7) return randomFromArr(corruptionsU);
        break;
      case "y":
        if (Math.random() < 0.7) return randomFromArr(corruptionsY);
        break;
      case "s":
        if (Math.random() < 0.7) return randomFromArr(corruptionsS);
    }
  }
  return char;
};

const corruptText = (str: string) => {
  return str
    .split("")
    .map((char: string) => corrupt(char.toLocaleLowerCase()))
    .join("");
};

const genColor = () => {
  const sat = Math.floor(Math.random() * 100);
  const lum = Math.floor(Math.random() * 120) + 20;
  const hue = 120;
  return `hsl(${hue}, ${sat}%, ${lum}%)`;
};

const genFont = () => {
  return randomFromArr([
    "Times New Roman",
    "Times",
    "serif",
    "Arial",
    "Helvetica",
    "sans-serif",
    "Georgia",
    "Courier New",
    "Courier",
    "monospace",
  ]);
};

const stylizeText = (): CSSProperties => {
  return {
    display: "inline-block",
    position: "absolute",
    color: genColor(),
    fontFamily: genFont(),
    fontSize: `${Math.round(genRandomNumBetween(1.0, 3.0))}em`,
    left: `${Math.round(genRandomNumBetween(0.1, 80))}%`,
    top: `${Math.round(genRandomNumBetween(6, 80))}%`,
    zIndex: 11,
    textShadow: `2px 0 0 #000,
                -2px 0 0 #000,
                0 2px 0 #000,
                0 -2px 0 #000,
                1px 1px #000,
                -1px -1px 0 #000,
                1px -1px 0 #000,
                -1px 1px 0 #000`,
  };
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

export { corruptText, stylizeImg, stylizeText };
