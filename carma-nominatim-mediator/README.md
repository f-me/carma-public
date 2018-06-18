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

- It starts HTTP server on port and host specified in `app.cfg` config and
  handles few routes you could look at in [Main](app/Main.hs) module;

- Handles requests to the Nominatim in separated thread which controls that
  next request will be triggered only after minimal inverval specified in
  `app.cfg` config;

- Caches successful responses from Nominatim associated with request data;

- Stores cached responses to a file from time to time (file and intervals are
  specified in `app.cfg` config);

- If cache file exists it will be loaded when app starts;

- From time to time it runs GC check which removes outdated cached responses
  (how long single cached response lives is specified in `app.cfg` config).

## Swagger API

You can get generated Swagger spec by this command:

```bash
curl 'http://127.0.0.1:8165/debug/swagger.json'
```

## Statistics

This microservice collects some statistics about requests it's handling.
You could obtain this statistics in JSON format by this command:

```bash
curl 'http://127.0.0.1:8165/debug/statistics'
```

You also able to process this data for specific purposes by for example `jq`
tool. Here is an example of getting hash-map where key is ISO 8601 date and
value is total requests count of "reverse-search" type of request:

```bash
curl -s 'http://127.0.0.1:8165/debug/statistics' | jq '[.[] | {(.iso_day): .by_request_type[] | select(.request_type == "reverse-search") | .statistics.total_requests}] | add'
```

An example of result:
```json
{
  "2018-06-18": 8,
  "2018-06-15": 1
}
```
