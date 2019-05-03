# CaRMa Mobile HTTP API

This API is usually used (but not limited) for creating mobile applications
which are supposed to send service requests.

All requests are supposed to be made using client SSL certificate as an
application identity. And __CaRMa Mobile API__ service is supposed to allow
requests only with provided client SSL certificate.

## Integration points

### Request for service

#### Request

- Route: `/geo/case`
- Method: __POST__
- Format: __JSON__
- Encoding: __UTF-8__
- In case of error: _returns HTTP status code 500_

| Field name              | Value example         | Required | Type                            | Description                                |
| ---                     | ---                   | :-:      | -                               | ---                                        |
| `car_vin`               | `"VFN1234567890ABCD"` | No       | string                          | VIN of a car                               |
| `car_plateNum`          | `"А228МР"`            | No       | string                          | Car license plate                          |
| `contact_phone1`        | `"+79152128506"`      | Yes      | string                          | Contact phone number                       |
| `contact_name`          | `"Н. О. Сопыркин"`    | No       | string                          | Name of a caller                           |
| `contact_email`         | `"halp@mail.ru"`      | No       | string                          | Contact Email                              |
| `cardNumber_cardNumber` | `"37218973"`          | No       | string                          | Program participant card number            |
| `lon`                   | `32.278`              | No       | number (fractional)             | Longitude of incident place (WGS84)        |
| `lat`                   | `50.2738`             | No       | number (fractional)             | Latitude of incident place (WGS84)         |
| `isAccident`            | `true`                | No       | boolean                         | Flag indicates whether it's road accident  |
| `program`               | `14`                  | No       | number (integer)[¹](#note-1)    | Program ID this service request belongs to |
| `subprogram`            | `88`                  | No       | number (integer)                | Subprogram ID                              |
| `car_make`              | `"ford"`              | No       | string [(dictionary)²](#note-2) | Car brand code                             |

#### Response

| Field name | Value example | Type             | Description                   |
| ---        | ---           | -                | ---                           |
| `caseId`   | `31337`       | number (integer) | ID of created service request |

#### Request example

_This is a pseudo-example, in real life you'd use client SSL certificate,
and the data in the response is also fake._

```bash
curl -XPOST https://mobile-api.carma-domain/geo/case -d'{"contact_phone1":"+71112223344"}'
```
It will respond with:
```json
{"caseId":123456}
```

\pagebreak

### Getting list of partners

_Usually used for an interactive map inside a mobile application._

- Route: `/geo/partnersAround/:coords/`
  (see available query-string params below)
    - `coords` param: coordinates of incident place in format `lon,lat`
      where `lon` and `lat` are fractional numbers
      ("**lon**gitude" and "**lat**itude" **coord**inates, WGS84)
- Method: __GET__
- Format: __JSON__
- Encoding: __UTF-8__
- In case of error: _returns HTTP status code 500_

#### Query-string params

_All params are optional (not required)._

| Field name | Value example | Type                            | Default     | Description                                |
| --         | --            | ---                             | -           | ----                                       |
| `car_make` | `ford`        | string [(dictionary)²](#note-2) | _any_       | Car brand code to filter by                |
| `limit`    | `5`           | number (integer)                | `20`        | Maximum amount of partners in a response   |
| `dist`     | `7`           | number (integer)                | _unlimited_ | Radius in kilometers to search partners in |

#### Request URL examples

- `https://mobile-api.carma-domain/geo/partnersAround/30.57,56.24/?car_make=citroen&dist=7`
- `https://mobile-api.carma-domain/geo/partnersAround/30.57,56.24/?limit=30`

#### Response

Returns JSON-array of another JSON-arrays with 8 elements
(`#` column indicates position of a field in an array,
it's not an index, index is one less):

| #  | Value example        | Type                | Description                                  |
| -: | -                    | -                   | -                                            |
| 1  | `757`                | number (integer)    | ID of a partner                              |
| 2  | `30.213`             | number (fractional) | Longitude of a partner location (WGS84)      |
| 3  | `56.213`             | number (fractional) | Latitude of a partner location (WGS84)       |
| 4  | `true`               | boolean             | Partner is a dealer (it's always `true` now) |
| 5  | `true`               | boolean             | It's a mobile partner                        |
| 6  | `"Dag avto"`         | string              | Name of a partner                            |
| 7  | `"ул. Гастелло, 42"` | string              | Address of a partner location                |
| 8  | `"+74952820512"`     | string              | Phone                                        |

#### Request example

_This is a pseudo-example, in real life you'd use client SSL certificate,
and the data in the response is also fake._

```bash
curl https://mobile-api.carma-domain/geo/partnersAround/30.57,56.24/?limit=2
```
It will respond with:
```json
[
  [12, 30.213, 56.0027, true, true, "Partner One", "ул. Такая, 31", "+7123"],
  [23, 30.8972, 55.321, true, false, "Peugeot Russie", "ул. Сякая, 38", "+7456"]
]
```

\pagebreak

## Dictionaries

### <a name="car-makers-dict"></a>Car Makers

Supposed to be statically embedded to a mobile application.

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Alfa Romeo             | `alfa`                   |
| Audi                   | `audi`                   |
| Bentley                | `bentley`                |
| BMW                    | `bmw`                    |
| Cadillac               | `cad`                    |
| Chevrolet              | `chevy`                  |
| Chrysler               | `chrysler`               |
| Citroen                | `citroen`                |
| Daewoo                 | `daewoo`                 |
| Dodge                  | `dodge`                  |
| Fiat                   | `fiat`                   |
| Ford                   | `ford`                   |
| Honda                  | `honda`                  |
| Hummer                 | `hum`                    |
| Hyundai                | `hyundai`                |
| Jaguar                 | `jaguar`                 |
| Jeep                   | `jeep`                   |
| Kia                    | `kia`                    |
| Land Rover             | `land`                   |
| Lexus                  | `lexus`                  |
| Mazda                  | `mazda`                  |
| Mercedes-Benz          | `mercedes`               |
| MINI                   | `mini`                   |
| Mitsubishi             | `mitsubishi`             |
| Nissan                 | `nissan`                 |
| Opel                   | `opel`                   |
| Peugeot                | `peugeot`                |
| Renault                | `renault`                |
| Saab                   | `saab`                   |
| Seat                   | `seat`                   |
| Skoda                  | `skoda`                  |
| Smart                  | `smart`                  |
| Subaru                 | `subaru`                 |
| Suzuki                 | `suzuki`                 |
| Toyota                 | `toyota`                 |
| Volkswagen             | `vw`                     |
| Volvo                  | `volvo`                  |
| ВАЗ (Lada)             | `lada`                   |

## Notes

<a name="note-1">¹</a> For compatibility instead of an integer number it's
allowed to pass a string `"ramc2"` or `"Cadillac"`.

<a name="note-2">²</a> See [Car Makers](#car-makers-dict) dictionary
for possible values.
