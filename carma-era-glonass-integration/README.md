# carma-era-glonass-integration *microservice*

Integration between CaRMa and Era Glonass.

## What is Ega Glonass?

Wikipedia article (russian one, there's no english version for now):
- https://ru.wikipedia.org/wiki/%D0%AD%D0%A0%D0%90-%D0%93%D0%9B%D0%9E%D0%9D%D0%90%D0%A1%D0%A1

It's kinda like russian version of the ECall system:
- https://en.wikipedia.org/wiki/ECall

Short and simplified description is:

A system that is integrated to a car to provide rapid emergency assistance.

## Testing

To run all tests related to this microservice just run:

```bash
tools/builder.sh -b backend-test-era-glonass-integration
```

## Documentation

To generate **haddock** documentation run:

```bash
stack exec -- haddock `find carma-era-glonass-integration/src/ -name '*.hs' | xargs` --html --hyperlinked-source -o haddock
```

And to open generated docs in browser:

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
curl -v '/debug/failures/count.json' | jq
```

### To see last 10 registered failures incidents

```bash
curl -v '/debug/failures/list.json?limit=10' | jq
```

### To see how many background tasks (threads) are currently running

```bash
curl -v 'http://127.0.0.1:8166/debug/background-tasks/count.json' | jq
```

## TODO

- Add more description to this document
