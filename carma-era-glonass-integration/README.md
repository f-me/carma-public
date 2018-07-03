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
(keep in mind that `carma-era-glonass-integration` server must be run to do so):

```bash
stack test :carma-era-glonass-integration-simulate-create-call-card
```

## TODO

- Add more description to this document
