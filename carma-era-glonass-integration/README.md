# carma-era-glonass-integration *microservice*

Integration between CaRMa and Era Glonass.

## What is Ega Glonass?

Wikipedia article (russian one, there's no english version for now):
- https://ru.wikipedia.org/wiki/%D0%AD%D0%A0%D0%90-%D0%93%D0%9B%D0%9E%D0%9D%D0%90%D0%A1%D0%A1

It's kinda like russian version of the ECall system:
- https://en.wikipedia.org/wiki/ECall

Short and simplified description is:

A system that is integrated to a car to provide rapid emergency assistance.

# Testing

You could simulate *EG.CRM.01* request from Era Glonass by this command
(it will run testing server with mocked in-memory SQLite database inside):

```bash
stack build && stack test :carma-era-glonass-integration-simulate-create-call-card
```

You also could run this simulation without mocking database, to make request to
existing server which works on real database (may be useful for development
purpuses) by using environment variable:

```bash
stack build && env CARMA_EG_TEST_WITHOUT_TESTING_SERVER=Y stack test :carma-era-glonass-integration-simulate-create-call-card
```

To run all tests related to this microservice just run:

```bash
tools/builder.sh -b backend-test-era-glonass-integration
```

# Documentation

To generate **haddock** documentation run:

```bash
stack exec -- haddock `find carma-era-glonass-integration/src/ -name '*.hs' | xargs` --html --hyperlinked-source -o haddock
```

## TODO

- Add more description to this document
