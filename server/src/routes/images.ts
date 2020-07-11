import express from "express";
import multer from "multer";
import { postImage, getImages } from "../controllers/imageController";
import { isBanned } from "../utils/postUtils";

const router = express.Router();

const upload = multer({ dest: "./src/temp" });

//post images
router.post("/", isBanned, upload.single("img"), (req, res) => {
  if (req.body.constructor === Object && Object.keys(req.body).length === 0)
    return res.send({ reason: "empty", status: 400 });

  if (!req.file) return res.send({ reason: "empty", status: 400 });

  postImage(req, res);
});

// get images
router.get("/", (req, res) => {
  getImages(req, res);
});

export = router;
