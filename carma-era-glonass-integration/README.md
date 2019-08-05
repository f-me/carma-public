# carma-era-glonass-integration *microservice*

Integration between CaRMa and Era Glonass.

## What is Ega Glonass?

[Wikipedia article][wiki-eg] (Russian one, there's no English version for now).
It's kind of like Russian version of the [ECall][wiki-ecall] system.

### Short and simplified description

A system that is integrated to a car to provide rapid emergency assistance.

## Testing

To run all the tests related to this microservice just run:

```bash
tools/builder.sh -b backend-test-era-glonass-integration
```

## Deployment

1. Build whole backend first (which includes building of this microservice):

    ```bash
    tools/builder.sh -b backend
    ```

    Or build it with `--production` flag if you're about to run it on production
    server (see [main README](../README.md) for details related to
    `builder.sh`).

2. Patch the configuration file (`carma-era-glonass-integration/app.cfg`)
    if needed

3. Run the microservice (you should be in `carma-era-glonass-integration`
    directory, not in root of the project):

    ```bash
    stack exec carma-era-glonass-integration
    ```

    Or if you're running it on production you probably have this binary in your
    `$PATH` but you have to specify proper working directory (where the
    microservice can find its configuration file), like this:

    ```bash
    (cd ~/carma/carma-era-glonass-integration && carma-era-glonass-integration)
    ```

    But you'd rather write an init-file and make a system service of it, and
    just restart this microservice like this:

    ```bash
    service carma-era-glonass-integration restart
    ```

## Logging

Currently it's just writing everything to _stdout_ using
[MonadLogger][monad-logger] by wrapping logger endpoint with
`runStdoutLoggingT` in [`Carma.EraGlonass.App`](src/Carma/EraGlonass/App.hs)
module. It's centralized log sink so some another flow of log writing can be
added there as an alternative.

__TODO__ Add writing to **syslog** as an alternative which could be set up in
the configuration file.

## Generating of documentation

To generate **haddock** documentation run:

```bash
stack exec -- haddock `find carma-era-glonass-integration/src/ -name '*.hs' | xargs` --html --hyperlinked-source -o haddock
```

And then to open generated docs in a browser run:

```bash
xdg-open haddock/index.html
```

## Useful commands

### To trigger VIN synchronization manually

```bash
curl -v 'http://127.0.0.1:8166/debug/vin-synchronizer/trigger.json' -XPOST | jq
```

### To see total count of registered failure incidents

```bash
curl -v 'http://127.0.0.1:8166/debug/failures/count.json' | jq
```

### To see last 10 registered failures incidents

```bash
curl -v 'http://127.0.0.1:8166/debug/failures/list.json?limit=10' | jq
```

### To see how many background tasks (threads) are currently running

```bash
curl -v 'http://127.0.0.1:8166/debug/background-tasks/count.json' | jq
```

[wiki-eg]: https://ru.wikipedia.org/wiki/%D0%AD%D0%A0%D0%90-%D0%93%D0%9B%D0%9E%D0%9D%D0%90%D0%A1%D0%A1
[wiki-ecall]: https://en.wikipedia.org/wiki/ECall
[monad-logger]: https://hackage.haskell.org/package/monad-logger
