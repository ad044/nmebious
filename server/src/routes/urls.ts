import express from "express";
import { postUrl } from "../controllers/urlController";
import { isBanned } from "../utils/postUtils";

const router = express.Router();

//post url
router.post("/", isBanned, async (req, res) => {
  if (req.body.constructor === Object && Object.keys(req.body).length === 0)
    return res.send({ reason: "empty", status: 400 });

  if (req.body.imgUrl == "") return res.send({ reason: "empty", status: 400 });

  postUrl(req, res);
});

export = router;
