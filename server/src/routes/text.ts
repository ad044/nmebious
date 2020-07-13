import express, { Request, Response } from "express";
const router = express.Router();
import { isBanned } from "../utils/postUtils";
import { postText, getText, getTextTillN } from "../controllers/textController";

//post text
router.post("/", isBanned, async (req: Request, res: Response) => {
  if (req.body.constructor === Object && Object.keys(req.body).length === 0)
    return res.status(400).send({ reason: "empty" });

  if (req.body.text == "") return res.status(400).send({ reason: "empty" });

  postText(req, res);
});

//get text
router.get("/", (req, res) => {
  getText(req, res);
});

// get text till n
router.get("/:n", (req, res) => {
  getTextTillN(req, res, parseInt(req.params.n));
});

export = router;
