import express from "express";
import bodyParser from "body-parser";
import cors from "cors";

import { createConnection } from "typeorm";

import Image from "./entities/Image";
import Text from "./entities/Text";
import Ban from "./entities/Ban";

import images = require("./routes/images");
import text = require("./routes/text");
import urls = require("./routes/urls");

const app = express();

app.use(bodyParser.json({ limit: "2mb" }));
app.use(bodyParser.urlencoded({ extended: false, limit: "2mb" }));

app.use(express.static(__dirname + "/uploads"));

app.use(cors());

app.use("/api/images", images);
app.use("/api/text", text);
app.use("/api/urls", urls);

createConnection({
  type: "postgres",
  host: "localhost",
  port: 5432,
  username: "postgres",
  password: "postgres",
  database: "mebi",
  synchronize: true,
  entities: [Image, Text, Ban],
  logging: false,
})
  .then(() => {
    console.log("Successfully connected.");
  })
  .catch((error) => console.log(error));

const port: number = 8080;
app.listen(port, () => console.log("Starting server on port 8000..."));

export = app;
