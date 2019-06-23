# CaRMa Mobile API Spec

This README file isn't a spec itself, to see actual spec go [here](Spec.md).

## Spec in markdown

[Spec.md](Spec.md)

## Automated actions

### To build markdown spec to all formats (PDF and HTML)

```bash
make
```

### To update car models of each car make from the database

```bash
./obtain-car-models-of-each-car-make.tcl postgresql://carma:pass@localhost/carma
```

Where `postgresql://carma:pass@localhost/carma` is a connection string to CaRMa
PostgreSQL database.

[Spec.md](Spec.md) will be automatically patched with new car models retrieved
from the database.

After that you may want to rerender spec to other formats:

```bash
make
```

# Authors

2019, Viacheslav Lotsmanov
