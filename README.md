# nmebious

nmebious (pronounced N-mebious) is an anonymous imageboard, a clone of [mebious](http://mebious.co.uk) written in Common Lisp using [Hunchentoot](https://edicl.github.io/hunchentoot/).

## Features

- Minimal and flexible API
- RSS feed
- Decoupled backend
- Easily extensible REPL administration tools

## Setting up

There are three main ways of setting up an instance:

1. Running `make`, which will build the binary and executing it.
2. Running `make run`, which will load the system and put you directly into the CL REPL. This will also create a SWANK server on port 4005 so you can connect and hack on it live.
3. Manually loading the system using `asdf:load-system` and running `(nmebious:start-server)`

Of course, before that we need to do some additional configuration:

- Install [SBCL](http://www.sbcl.org/) along with [Quicklisp](https://www.quicklisp.org/).
- PostgreSQL must be set up with 2 databases named `nmebious` and `test` (the second one is optional and only needed if you want to run the test suite).
- Create a `.env` file with the following format:
	```
   PGUSER=user
   PGPASSWORD=password
   SECRET=secret
   MAX_FILE_SIZE=2
   	```
  Where `PGUSER` and `PGPASSWORD` are the credentials for the Postgres database, `SECRET` is the key used by HMAC to hash the IP addresses, and `MAX_FILE_SIZE` is the maximum allowed file size in megabytes.

- [ImageMagick](https://imagemagick.org/) needs to be installed.

- It is HIGHLY recommended that you use [nginx](https://www.nginx.com/) on top of nmebious to handle rate limiting, static file serving, limiting the accepted file sizes, etc.

## Configuration

The configuration is located inside `src/config.lisp`.
The only thing that you need to change is the `*web-url*` parameter, which gets displayed on the RSS feed. Everything else you can modify/adjust to your liking, or just leave as defaults.

## Testing

Load the system and run `(asdf:test-system :nmebious)`.
One important thing to keep in mind is that the server MUST be off when running the tests.

  
