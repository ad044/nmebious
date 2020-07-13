A clone of [mebious](http://mebious.co.uk) written in TypeScript along with some additional functionality like live updates and url submission support.

Inspiration taken from another clone written in Ruby - https://github.com/ix/mebious/

## Dev setup
- Clone the repo.
- Move inside **/client/** and run `npm install`.
- Move inside **/server/** and run `npm install`.
- Start both of them by running `npm start` in each of these directories.

The server looks for a database named "mebi" on PostgreSQL.
Server runs on port `8080` while the clientside is hosted on port `3000`.
## Code overview
- **/server/** 
  - **src/controllers/** contains methods used by routes well as the API when handling posts. 
  - **src/entities/** contains models defined by TypeORM.
  - **src/routes/** contains express routes.
  - **src/utils/** contains all sorts of helper functions used by the server for stuff like form validation, downloading data and stuff.
  
  Images are stored as tempfiles in inside **src/temp/**.
  
  Permanent storage for formatted and validated images is **src/uploads/**.
- **/client/** 
  - **src/components/** contains React components.
  - **src/static/** contains static files, in this case only css.
  - **src/utils/** contains utils for Mebious to render/format data on the webpage.

# GET methods

`GET /api/text` -> Returns a JSON array of objects representing the last 10 text posts.

`GET /api/text/n` (n = Integer) -> Returns a JSON array of objects representing the last `n` text posts.

`GET /api/image` -> Returns a JSON array of objects representing the last 20 image posts.

`GET /api/image/n` (n = Integer) -> Returns a JSON array of objects representing the last `n` image posts.

# POST methods

Each of the POST requests return a JSON object of response success/error state.

 Request body examples:

`POST /api/text` -> `{ text: example }` 

`POST /api/url` ->  `{ imgUrl: example }` 

`POST /api/images` -> `{ file: example }` 

# Deps
- Node with Express framework for back-end.
- React for front-end.
- PostgreSQL for RDBMS.
- TypeORM as the ORM.

