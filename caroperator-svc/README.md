# caroperator-svc *microservice*

An integration between [caroperator.ru](http://caroperator.ru/) and CaRMa.

Partners from __caroperator__ are adding new contracts into our DB using this
microservice.

Spec (__TODO__ translate to English and put it to this repo as markdown doc):
https://docs.google.com/document/d/1nsgjiUg2UKklqpV3byJhTVqgCxo8BM6BtsUCxsEv1lo/edit

A guide to work with client SSL certificates
(__TODO__ translate to English and put it to this repo as markdown doc but to
general `docs` directory, it's not limited to this particular microservice):
https://docs.google.com/document/d/1PkbAMJ5zQ5odTkhuXfPO-QzZa72a09QNixD3ilKSBgs/edit#heading=h.ybdgzy7otdbm

## Requirements

See config file of this microservice.

- PostgreSQL DB user to `INSERT` into `Contract` table and `SELECT` from few
  another tables such as `CarModel`, `CarMake` and `partnertbl`;
- CaRMa user which plays `Contract` comitter role;
- CaRMa `SubProgram` for new added `Contract`s.

## Utilities

These utilities are provided just as an example.
Real configs and deployment process may vary.

- `sudo ./install.sh` − Copies configuration files and starts these services:

  + nginx (copies config for __caroperator-svc__ and restarts __nginx__)
  + syslog (copies config for __caroperator-svc__ and restarts __rsyslog__)
  + upstart job (copies config for __caroperator-svc__ and starts it)

- `sudo ./cert.sh init-ca` − Initializes certificates storage for certificates
  for client SSL authentication.

## TODO

[ ] `rate_limit` (on __nginx__ side, provide an example here and doc it
    properly)
