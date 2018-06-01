# carma-nominatim-mediator *microservice*

Middleware between Nominatim server and CaRMa.

## Purpose

Community shared Nominatim server have some limits such as minimal intervals
between requests (1 second, or you get banned for some period of time), it
requires *User-Agent* to be provided, etc. Running own instance could be
expensive because of relatively big requirements of CPU, RAM and disk space. So
this service was created to use shared server without caring about such limits,
to be able to use Nominatim freely and this tool supposed to care about handling
it satisfying those limits.

## Usage info

1. You must have `app.cfg` config file, if you don't have it, copy default one:

   ```bash
   cp app.cfg.default app.cfg
   ```

2. Run this microservice by this command (while being in
   `carma-nominatim-mediator` directory):

   ```bash
   stack exec carma-nominatim-mediator
   ```

   _P.S. Cache will be stored in file `cache-snapshot.db` in
   `carma-nominatim-mediator` directory by default, you can change it in
   `app.cfg` file._

3. Test if it works:

   ```bash
   curl -v 'http://127.0.0.1:8165/reverse-search/ru-RU,ru/52.51719785,13.3978352028938'
   ```

   _P.S. Port and host might be different depending on `app.cfg` config._

## How it works

- It handles requests to the Nominatim in separated thread which controls that
  next request will be triggered only after minimal inverval specified in
  `app.cfg` config;

- Caches successful responses from Nominatim associated with request data;

- Stores cached responses to a file from time to time (file and intervals are
  specified in `app.cfg` config);

- If cache file exists it will be loaded when app starts;

- From time to time it runs GC check which removed outdates cached responses
  (how long single cached response lives is specified in `app.cfg` config).

## TODO

- Return cached response immediately notwithstanding if new requests is
  processing. Currently if new requests is processing requests for cached
  response will wait until it finishes.
