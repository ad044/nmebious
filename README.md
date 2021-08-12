# nmebious

<p align="center">
  <img src="markdown/logo.png">
</p>

nmebious (pronounced N-mebious) is an anonymous imageboard, a clone of [mebious](http://mebious.co.uk) written in Common Lisp using [Hunchentoot](https://edicl.github.io/hunchentoot/).

## Features

- Minimal and flexible API
- Board support
- RSS feed
- Easily extensible REPL administration tools

## Setting up

There are four main ways of setting up an instance:

1. Running `make`, which will build the binary and executing it.
2. Running `make run`, which will load the system and put you directly into the CL REPL. 
3. Manually loading the system using `asdf:load-system` and running `(nmebious:start-server)`
4. Building and running via Docker using `docker-compose up`.

Note that running `(main)`, which is the main entrypoint of the application, also starts a SWANK server on port 4005, to which you can connect to and hack the system live.

Of course, before that we need to do some additional configuration:

- Install [SBCL](http://www.sbcl.org/) along with [Quicklisp](https://www.quicklisp.org/).

- Create a `.env` file with the following format:
	```
  POSTGRES_PASSWORD=postgres_password
  ADMIN_USERNAME=admin
  ADMIN_PASSWORD=admin_password
  MAX_FILE_SIZE=2
  ```
  Where `POSTGRES_PASSWORD` is the password for the Postgres database, `ADMIN_USERNAME` and `ADMIN_PASSWORD` serve as credentials for the admin panel, and `MAX_FILE_SIZE` is the maximum allowed file size in megabytes.  
  Note: Please choose strong and complicated passwords and not `admenistrator123`, especially for the `ADMIN_PASSWORD` field, which you'll use to moderate the website through `/admin/panel`.

- PostgreSQL must be set up with 2 databases named `nmebious` and `nmebious_test` (the second one is optional and only needed if you want to run the test suite). This can be automated by using the `init-db.sh` script located inside the `scripts` directory.

- [ImageMagick](https://imagemagick.org/) must be installed to format uploaded images.

- It is HIGHLY recommended that you use [nginx](https://www.nginx.com/) on top of nmebious to handle rate limiting, static file serving, limiting the accepted file sizes, etc. (You can find the default configuration that the Docker container uses inside the `nginx` directory)

## Configuration

The configuration is located inside `src/config.lisp`.
Here is a list of things that you'll likely want to modify or at least look into:
- `*web-url*` - The URL of the website where `nmebious` is being hosted (this will be displayed on the RSS feed).
- `*boards*` - Board names, backgrounds, colors. Must be 1 or more. You can opt to only have one board if you're going for a "classic" mebious look, or have more, in which case the default frontend will display a list of them. As for what the boards should be, you decide! They are just tools to separate context between each other, for example a `technology` board would contain posts on that topic, etc.
- `*api-requires-key*` - Whether or not the API for POSTing/GETting data will be open to the public. If set to `nil` (false), anyone will be able to access the api, if set to `t` (true), only those with an API key will be able to access it (You'll be able to manage these keys via the admin panel).
- `*socket-server-enabled*` - If enabled, a client that supports WebSockets will receive updates on new posts, which can be used to update the feed realtime. If you don't plan on using alternative frontends for your instance, you should probably set this to `nil`.
- `*allow-duplicates-after*` - A number stating how many unique posts a user can make until being allowed to post a duplicate. Set to `nil` if you want to allow duplicate posts instantly.
- `*filtered-words*` - Words that should be filtered, must be a list of strings, for example `'("filter1" "filter2")`

For more options look into the `config.lisp` file itself.

## API
All API routes return data in JSON format.

- POST `/api/submit/text` - Takes data of type `application/x-www-form-urlencoded` and responds with a status code and message.  
Must contain a `board` and `text`.
- POST `/api/submit/file` - Takes data of type `multipart/form-data` and responts with a status code and message.  
Must contain a `board` and `file`.
- GET `/api/posts/` - Returns the last 15 posts across all boards by default.  
Can take optional query parameters such as `count` and `offset`, where `count` denotes how many posts to GET and is <= `*post-get-limit*`, and `offset` denotes the `OFFSET` inside the SQL query.  
`count` defaults to 15, while `offset` defaults to 0.  
Example: `/api/posts/count=15&offset=30`
- GET `/api/posts/:board` - Behaves identically to a regular `/api/posts/` request, except it only returns posts from the specified board.
- GET `/api/config` - Responds with the server configuration.

## FAQ
- How to add custom fonts?  
Download your desired font, place it anywhere you want (naturally that would be `public/fonts`), then, in `public/css/fonts.css` declare it using `font-face` and add the `font-family` as a string to the parameter `*fonts*` located inside `config.lisp`.
- Docker vs manual deployment?  
Probably Docker.
- Why rewrite from TS?  
Alerts from dependabot were driving me crazy, also the old implementation was weird and hacky.
- Why CL?  
Old and stable language, for fun, and, most importantly, Lain loves Common Lisp.

## Testing

Load the system and run `(asdf:test-system :nmebious)`.
One important thing to keep in mind is that the server MUST be off when running the tests.

## References
[Rose's mebious clone](https://github.com/dataphreak1001/mebious) - Used as reference for lots of stuff, and default frontend code.