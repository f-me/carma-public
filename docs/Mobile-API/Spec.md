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
| `car_model`             | `"ikon"`              | No       | string [(dictionary)³](#note-3) | Car model code                             |

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

*Value* is case-sensitive!

Each **Car Maker** has own limited possible values for `car_model` field.
See [Car Models](#car-models) section for details.

[//]: # (#CAR_MAKERS_TABLE# -- separator for automation scripts)
| Label (human-readable) | Value (used in requests) | Car Models                                        |
| -                      | -                        | -                                                 |
| Alfa Romeo             | `alfa`                   | [Dictionary](#car-models-of-alfa-car-maker)       |
| Audi                   | `audi`                   | [Dictionary](#car-models-of-audi-car-maker)       |
| Bentley                | `bentley`                | [Dictionary](#car-models-of-bentley-car-maker)    |
| BMW                    | `bmw`                    | [Dictionary](#car-models-of-bmw-car-maker)        |
| Cadillac               | `cad`                    | [Dictionary](#car-models-of-cad-car-maker)        |
| Chevrolet              | `chevy`                  | [Dictionary](#car-models-of-chevy-car-maker)      |
| Chrysler               | `chrysler`               | [Dictionary](#car-models-of-chrysler-car-maker)   |
| Citroen                | `citroen`                | [Dictionary](#car-models-of-citroen-car-maker)    |
| Daewoo                 | `daewoo`                 | [Dictionary](#car-models-of-daewoo-car-maker)     |
| Dodge                  | `dodge`                  | [Dictionary](#car-models-of-dodge-car-maker)      |
| Fiat                   | `fiat`                   | [Dictionary](#car-models-of-fiat-car-maker)       |
| Ford                   | `ford`                   | [Dictionary](#car-models-of-ford-car-maker)       |
| Honda                  | `honda`                  | [Dictionary](#car-models-of-honda-car-maker)      |
| Hummer                 | `hum`                    | [Dictionary](#car-models-of-hum-car-maker)        |
| Hyundai                | `hyundai`                | [Dictionary](#car-models-of-hyundai-car-maker)    |
| Jaguar                 | `jaguar`                 | [Dictionary](#car-models-of-jaguar-car-maker)     |
| Jeep                   | `jeep`                   | [Dictionary](#car-models-of-jeep-car-maker)       |
| Kia                    | `kia`                    | [Dictionary](#car-models-of-kia-car-maker)        |
| Land Rover             | `land`                   | [Dictionary](#car-models-of-land-car-maker)       |
| Lexus                  | `lexus`                  | [Dictionary](#car-models-of-lexus-car-maker)      |
| Mazda                  | `mazda`                  | [Dictionary](#car-models-of-mazda-car-maker)      |
| Mercedes-Benz          | `mercedes`               | [Dictionary](#car-models-of-mercedes-car-maker)   |
| MINI                   | `mini`                   | [Dictionary](#car-models-of-mini-car-maker)       |
| Mitsubishi             | `mitsubishi`             | [Dictionary](#car-models-of-mitsubishi-car-maker) |
| Nissan                 | `nissan`                 | [Dictionary](#car-models-of-nissan-car-maker)     |
| Opel                   | `opel`                   | [Dictionary](#car-models-of-opel-car-maker)       |
| Peugeot                | `peugeot`                | [Dictionary](#car-models-of-peugeot-car-maker)    |
| Renault                | `renault`                | [Dictionary](#car-models-of-renault-car-maker)    |
| Saab                   | `saab`                   | [Dictionary](#car-models-of-saab-car-maker)       |
| Seat                   | `seat`                   | [Dictionary](#car-models-of-seat-car-maker)       |
| Skoda                  | `skoda`                  | [Dictionary](#car-models-of-skoda-car-maker)      |
| Smart                  | `smart`                  | [Dictionary](#car-models-of-smart-car-maker)      |
| Subaru                 | `subaru`                 | [Dictionary](#car-models-of-subaru-car-maker)     |
| Suzuki                 | `suzuki`                 | [Dictionary](#car-models-of-suzuki-car-maker)     |
| Toyota                 | `toyota`                 | [Dictionary](#car-models-of-toyota-car-maker)     |
| Volkswagen             | `vw`                     | [Dictionary](#car-models-of-vw-car-maker)         |
| Volvo                  | `volvo`                  | [Dictionary](#car-models-of-volvo-car-maker)      |
| ВАЗ (Lada)             | `lada`                   | [Dictionary](#car-models-of-lada-car-maker)       |

\pagebreak

### <a name="car-models"></a>Car Models

Supposed to be statically embedded to a mobile application.

*Value* is case-sensitive!

This list may be extended in the future.

[//]: # (WARNING! This list is produced automatically, see README.md for details, do not edit it manually!)
[//]: # (#CAR_MODELS_BEGIN# -- separator for automation scripts)
#### <a name="car-models-of-alfa-car-maker"></a>Car Models of "Alfa Romeo" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 147                    | alfa147                  |
| 159                    | alfa159                  |
| 166                    | alfa166                  |
| Brera                  | brera                    |

\pagebreak

#### <a name="car-models-of-audi-car-maker"></a>Car Models of "Audi" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 80                     | 80                       |
| 90                     | 90                       |
| A1                     | A1                       |
| A2                     | a2                       |
| A3                     | a3                       |
| A4                     | a4                       |
| A5                     | A5                       |
| A6                     | a6                       |
| A7                     | a7                       |
| A8                     | a8                       |
| Allroad                | Allroad                  |
| e-tron                 | e-tron                   |
| Q2                     | q2                       |
| Q3                     | Q3                       |
| Q5                     | Q5                       |
| Q7                     | Q7                       |
| Q8                     | q8                       |
| R8                     | r8                       |
| RS4                    | rs4                      |
| RS5                    | rs5                      |
| RS6                    | rs6                      |
| RS7                    | rs7                      |
| S3                     | s3                       |
| S4                     | s4                       |
| S5                     | S5                       |
| S7                     | s7                       |
| TT                     | tt                       |

\pagebreak

#### <a name="car-models-of-bentley-car-maker"></a>Car Models of "Bentley" Car Maker

| Label (human-readable)    | Value (used in requests) |
| -                         | -                        |
| Arnage                    | arnage                   |
| Bentayga /Бэнтайга        | Bentayga                 |
| Continental Flying Spur   | continental              |
| Continental /Континенталь | Continental              |
| Mulsanne /Мульсан         | Mulsanne                 |

\pagebreak

#### <a name="car-models-of-bmw-car-maker"></a>Car Models of "BMW" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 1 series               | 1s                       |
| 2 Series               | 2series                  |
| 3 series               | 3s                       |
| 4 series               | 4s                       |
| 5 series               | 5s                       |
| 6 series               | 6s                       |
| 7 series               | 7s                       |
| 8 series               | 8s                       |
| i3 / ай 3              | i3                       |
| i5 / ай 5              | i5                       |
| i8 / ай 8              | i8                       |
| M2                     | m2                       |
| M3                     | m3                       |
| M4                     | 4m                       |
| M5                     | m5                       |
| M6                     | m6                       |
| X1                     | x1                       |
| X2                     | x2                       |
| X3                     | x3                       |
| X4                     | x4                       |
| X5                     | x5                       |
| X6                     | x6                       |
| X7                     | x7                       |
| xActivity              | xActivity                |
| Z1                     | z1                       |
| Z3                     | z3                       |
| Z4                     | z4                       |
| Z8                     | z8                       |

\pagebreak

#### <a name="car-models-of-cad-car-maker"></a>Car Models of "Cadillac" Car Maker

| Label (human-readable)       | Value (used in requests) |
| -                            | -                        |
| Allante                      | allante                  |
| ATS/Эй Ти Эс                 | ats                      |
| BLS                          | bls                      |
| Brougham                     | brougham                 |
| Catera                       | catera                   |
| CT6 /Си Ти 6                 | ct6                      |
| CTS/Си Ти Эс                 | cts                      |
| DE Ville                     | ville                    |
| DTS                          | dts                      |
| Eldorado                     | eldorado                 |
| Escalade/Эскалэйд            | escalade                 |
| Fleetwood                    | fleetwood                |
| LSE                          | lse                      |
| Seville                      | seville                  |
| SRX/XT5   Эс Эр Икс/Икс Ти 5 | srx                      |
| STS/Эс Ти Эс                 | sts                      |
| XLR                          | xlr                      |
| XT5 /Икс Ти 5                | xt5                      |
| XTS/Икс Ти Эс                | xts                      |

\pagebreak

#### <a name="car-models-of-chevy-car-maker"></a>Car Models of "Chevrolet" Car Maker

| Label (human-readable)     | Value (used in requests) |
| -                          | -                        |
| Alero/Алеро                | alero                    |
| Avalanche                  | avalanche                |
| Aveo/Авео                  | aveo                     |
| Beretta/Беретта            | beretta                  |
| Blazer/Блэйзер             | blazer                   |
| Camaro/Камаро              | camaro                   |
| Caprice/Каприс             | capriceCh                |
| Captiva/Каптива            | captiva                  |
| Cavalier                   | cavalier                 |
| Celta                      | celta                    |
| Cheyenne                   | cheyenne                 |
| Cobalt/Кобальт             | cobaltCh                 |
| Colorado/Колорадо          | colorado                 |
| Corsa Wind                 | wind                     |
| Corsica                    | corsica                  |
| Corvette/Корвет            | corvette                 |
| Cruiser                    | Cruiser                  |
| Cruze/Круз                 | cruze                    |
| Epica/Эпика                | epicaCh                  |
| Evanda                     | evanda                   |
| Express                    | express                  |
| HHR                        | hhr                      |
| Impala                     | impala                   |
| Impala SS                  | impalaSS                 |
| Ipanema GL                 | ipanemaGL                |
| Jimmy                      | jimmy                    |
| Lacetti/Лачетти            | lacetti                  |
| Lanos/Ланос                | lanosCh                  |
| Lumina                     | lumina                   |
| Malibu                     | malibu                   |
| Metro                      | metro                    |
| Monte Carlo                | monteCarlo               |
| Monza                      | monza                    |
| NIVA/Нива                  | niva                     |
| Orlando/Орландо            | orlando                  |
| Prism                      | prism                    |
| Rezzo/Резо                 | Rezzo                    |
| S-10                       | s10                      |
| Spark/Спарк                | spark                    |
| SS                         | ss                       |
| Suburban                   | suburban                 |
| Suburban/Сэбёрбэн          | suburban                 |
| Tacuma                     | tacuma                   |
| Tahoe/Тахо                 | tahoe                    |
| Tracker Convertible        | trackerConv              |
| Tracker Hardtop            | trackerHard              |
| Tracker/Трэкер             | tracker                  |
| Trail Blazer/Трэйл Блэйзер | trailBlazer              |
| Trans Sport                | Trans                    |
| Traverse/Траверс           | traverse                 |
| Trax/Трэкс                 | trax                     |
| Venture                    | venture                  |
| Viva                       | viva                     |

\pagebreak

#### <a name="car-models-of-chrysler-car-maker"></a>Car Models of "Chrysler" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 300                    | chrysler300              |
| Grand Voyager          | Grand Voyager            |
| Pacifica               | pacifica                 |
| Sebring                | sebring                  |

\pagebreak

#### <a name="car-models-of-citroen-car-maker"></a>Car Models of "Citroen" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Berlingo First         | berlingo1st              |
| Berlingo/Берлинго      | berlingo                 |
| C1                     | c1                       |
| C15                    | c15                      |
| C2                     | c2                       |
| C3                     | c3                       |
| C3 AIRCROSS            | c3_aircross              |
| C3 Picasso/Ц 3         | c3Picasso                |
| C3 Pluriel             | c3Pluriel                |
| C4                     | c4                       |
| C4 Aircross            | c4Aircross               |
| C4 Grand Picasso       | c4GrandPicasso           |
| C4L                    | C4L                      |
| C4 Picasso             | c4Picasso                |
| C5                     | c5                       |
| C6                     | c6                       |
| C8                     | c8                       |
| C-Crosser              | cCrosser                 |
| C-Elysee/Ц-Элизэ       | C-Elysee                 |
| C-Zero                 | cZero                    |
| DS3                    | ds3                      |
| DS4                    | ds4                      |
| DS5                    | ds5                      |
| DS7 CROSSBACK          | ds7_crossback            |
| Evasion/Jumpy          | evasionJumpy             |
| Jumper                 | jumper                   |
| Jumpy                  | jumpy                    |
| Nemo                   | nemo                     |
| Saxo                   | saxo                     |
| SpaceTourer            | spacetourer              |
| Xsara                  | xsara                    |
| Xsara Picasso          | xsaraPicasso             |
| ZX                     | zx                       |

\pagebreak

#### <a name="car-models-of-daewoo-car-maker"></a>Car Models of "Daewoo" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Chairman               | chairman                 |
| Espero                 | espero                   |
| Gentra                 | gentra                   |
| Matiz                  | matiz                    |
| Nexia                  | nexia                    |
| Tosca                  | tosca                    |

\pagebreak

#### <a name="car-models-of-dodge-car-maker"></a>Car Models of "Dodge" Car Maker

| Label (human-readable)             | Value (used in requests) |
| -                                  | -                        |
| Avenger (Додж Авенджер)            | avenger                  |
| Caliber (Додж Калибр)              | caliber                  |
| Caravan                            | caravan                  |
| Challenger                         | challenger               |
| Charger                            | charger                  |
| Dakota                             | dakota                   |
| Durango                            | durango                  |
| Grand Caravan (Додж Гранд Караван) | grandcaravan             |
| Intrepid                           | intrepid                 |
| Journey                            | journey                  |
| Journey (Додж Джорни)              | Journey                  |
| Magnum                             | magnum                   |
| Neon                               | neon                     |
| Nitro                              | nitro                    |
| RAM 1500 (Додж Рам 1500)           | RAM 1500                 |
| Stealth                            | stealth                  |
| Stratus                            | stratus                  |
| Viper                              | viper                    |

\pagebreak

#### <a name="car-models-of-fiat-car-maker"></a>Car Models of "Fiat" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 500                    | 500                      |
| Albea                  | albea                    |
| Bravo                  | bravo                    |
| Doblo                  | Doblo                    |
| Ducato                 | Ducato                   |
| Linea                  | Linea                    |
| Panda                  | panda                    |
| Punto                  | punto                    |
| Scudo                  | scudo                    |
| Sedici                 | sedici                   |

\pagebreak

#### <a name="car-models-of-ford-car-maker"></a>Car Models of "Ford" Car Maker

| Label (human-readable)         | Value (used in requests) |
| -                              | -                        |
| 427                            | ford427                  |
| Aerostar/Аэростар              | aerostar                 |
| Aspire/Эспайэ                  | aspire                   |
| B-Max/Би-Макс                  | bMax                     |
| Bronco/Бронко                  | bronco                   |
| C-Max II                       | cMaxII                   |
| C-Max/Си-Макс                  | cMax                     |
| Contour                        | contour                  |
| Cougar                         | cougar                   |
| Courier                        | COURIER                  |
| Crown Victoria                 | crownVictoria            |
| Econoline                      | econoline                |
| EcoSport /ЭкоСпорт             | EcoSport                 |
| Edge/Эдж                       | fordEdge                 |
| Escape/Эскейп                  | escape                   |
| Escort                         | escort                   |
| Escort Cabrio                  | escortCabrio             |
| Escort Classic                 | escortClassic            |
| Escort Estate                  | escortEstate             |
| Escort Hatchback               | escortHatchback          |
| Escort Turnier                 | escortTurnier            |
| Escort ZX2                     | escortZX2                |
| Excursion/Экскёршн             | excursion                |
| Expedition/Экспедишн           | expedition               |
| Explorer/Эксплорер             | explorer                 |
| F150                           | f150                     |
| F250                           | f250                     |
| Faction                        | faction                  |
| Fairlane                       | fairlane                 |
| Falcon GT                      | falconGT                 |
| Fiesta/Фиеста                  | fiesta                   |
| Focus/Фокус                    | focus                    |
| Fusion/Фьюжн                   | fusion                   |
| Galaxy/Гэлакси                 | galaxy                   |
| GRAND C-Max/Гранд Си-Макс      | grandCMax                |
| GT/Джи Ти                      | fordGT                   |
| Ikon                           | ikon                     |
| Ka/Ка                          | ka                       |
| Kuga/Куга                      | kuga                     |
| LTD                            | ltd                      |
| Maverick/Мэвэрик               | maverick                 |
| Model U/Модель У               | modelU                   |
| Mondeo/Мандэо                  | mondeo                   |
| Mustang/Мустанг                | mustang                  |
| Oldtimer                       | Oldtimer                 |
| Probe                          | probe                    |
| Puma                           | puma                     |
| Ranger/Рэнджер                 | ranger                   |
| Scorpio                        | scorpio                  |
| Shelby GR                      | shelbyGR                 |
| Sierra                         | sierra                   |
| S-Max/Эс-Макс                  | sMax                     |
| SportKa/Спорт Ка               | sportKa                  |
| StreetKa/Стрит Ка              | streetKa                 |
| Taurus                         | taurus                   |
| Thunderbird                    | thunderbird              |
| Tourneo Connect/Торнео Коннект | tourenoConnect           |
| Tourneo Custom / Торнео Кастом | tourneo_custom           |
| Transit/Транзит                | transit                  |
| US Vehicles/Ю Эс Виэклс        | usVehicles               |
| Windstar                       | windstar                 |

\pagebreak

#### <a name="car-models-of-honda-car-maker"></a>Car Models of "Honda" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Accord                 | accord                   |
| Accord Inspire         | accordinspire            |
| Acty                   | acty                     |
| Acty Truck             | actytruck                |
| Airwave                | airwave                  |
| Airweive               | airweive                 |
| Ascot                  | ascot                    |
| Ascot Innova           | ascotinnova              |
| Avancier               | avancier                 |
| Beat                   | beat                     |
| Capa                   | capa                     |
| City                   | city                     |
| Civic                  | civic                    |
| Civic Ferio            | civicferio               |
| Civic Shuttle          | civicshuttle             |
| Civic Type R           | civictyper               |
| Clarity                | clarity                  |
| Concerto               | concerto                 |
| Crossroad              | crossroad                |
| Crosstour              | crosstour                |
| CR-V                   | CR-V                     |
| CR-X                   | crx                      |
| CR-X del Sol           | crxdelsol                |
| CR-Z                   | crz                      |
| Domani                 | domani                   |
| Edix                   | edix                     |
| Element                | Element                  |
| Elysion                | elysion                  |
| Fit                    | fit                      |
| Fit Aria               | fitaria                  |
| Fit Shuttle            | fitshuttle               |
| Freed                  | freed                    |
| Freed+                 | freed                    |
| Freed Spike            | freedspike               |
| FR-V                   | frv                      |
| Grace                  | grace                    |
| Horizon                | horizon                  |
| HR-V                   | hrv                      |
| Insight                | insight                  |
| Inspire                | inspire                  |
| Integra                | integra                  |
| Integra SJ             | integrasj                |
| Jade                   | jade                     |
| Jazz                   | jazz                     |
| Lagreat                | lagreat                  |
| Legend                 | legend                   |
| Life                   | life                     |
| Life Dunk              | lifedunk                 |
| Logo                   | logo                     |
| Mobilio                | mobilio                  |
| Mobilio Spike          | mobiliospike             |
| N-BOX                  | nbox                     |
| N-BOX+                 | nbox                     |
| N-BOX Slash            | nboxslash                |
| N-ONE                  | none                     |
| NSX                    | nsx                      |
| N-VAN                  | nvan                     |
| N-WGN                  | nwgn                     |
| Odyssey                | odyssey                  |
| Orthia                 | orthia                   |
| Partner                | partner                  |
| Pilot                  | Pilot                    |
| Prelude                | prelude                  |
| Quintet                | quintet                  |
| Rafaga                 | rafaga                   |
| Ridgeline              | ridgeline                |
| S2000                  | s2000                    |
| S660                   | s660                     |
| Saber                  | saber                    |
| Shuttle                | shuttle                  |
| S-MX                   | smx                      |
| StepWGN                | StepWGN                  |
| Stream                 | stream                   |
| Street                 | street                   |
| Thats                  | thats                    |
| Today                  | today                    |
| Torneo                 | torneo                   |
| Vamos                  | vamos                    |
| Vamos Hobio            | vamoshobio               |
| Vezel                  | vezel                    |
| Vigor                  | vigor                    |
| Z                      | z                        |
| Zest                   | zest                     |

\pagebreak

#### <a name="car-models-of-hum-car-maker"></a>Car Models of "Hummer" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| H1                     | h1                       |
| H2                     | h2                       |
| H3                     | h3                       |

\pagebreak

#### <a name="car-models-of-hyundai-car-maker"></a>Car Models of "Hyundai" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Accent                 | accent                   |
| Atos                   | atos                     |
| Centennial             | centennial               |
| Chery QQ (Sweet)       | cheryQQ                  |
| Creta                  | Creta                    |
| Elantra                | elantra                  |
| Equus                  | equus                    |
| Genesis                | genesis                  |
| Getz                   | getz                     |
| Grandeur               | grandeur                 |
| Grand Santa Fe         | grand santa fe           |
| H-1 Starex             | H-1 Starex               |
| HD-65                  | hd65                     |
| HD-78                  | hd78                     |
| i10                    | i10                      |
| i20                    | i20                      |
| i30                    | i30                      |
| i40                    | i40                      |
| IX35                   | IX35                     |
| IX55                   | IX55                     |
| Matrix                 | Matrix                   |
| Porter                 | Porter                   |
| Santa Fe new           | santaFe                  |
| Solaris                | solaris                  |
| Sonata                 | sonata                   |
| Tucson                 | Tucson                   |
| Veloster               | veloster                 |
| Verna                  | verna                    |
| XG                     | xg                       |

\pagebreak

#### <a name="car-models-of-jaguar-car-maker"></a>Car Models of "Jaguar" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Daimler                | Daimler                  |
| E-Pace                 | epace                    |
| F-Pace                 | fpace                    |
| F-Type                 | ftype                    |
| I-Pace                 | ipace                    |
| S-Type                 | sType                    |
| XE                     | xe                       |
| XF                     | xf                       |
| XJ                     | xj                       |
| X-Type                 | xType                    |

\pagebreak

#### <a name="car-models-of-jeep-car-maker"></a>Car Models of "Jeep" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Cherokee               | Cherokee                 |
| Commander              | commander                |
| Compass                | Compass                  |
| Grand Cherokee         | Cherokee                 |

\pagebreak

#### <a name="car-models-of-kia-car-maker"></a>Car Models of "Kia" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Carens                 | carens                   |
| Carens/Карэнс          | Carens                   |
| Carnival/Карнивэл      | Carnival                 |
| Cee'd/Сид              | ceed                     |
| Cerato/Церато          | cerato                   |
| K900                   | k900                     |
| Magentis/Маджентис     | magentis                 |
| Mentor/Ментор          | mentor                   |
| Mohave/Махавэ          | mohave                   |
| Opirus/Опирэс          | opirus                   |
| Optima/Оптима          | optima                   |
| Picanto/Пиканто        | picanto                  |
| Pro Ceed/Про Сид       | ProCeed                  |
| Quoris/Кворис          | quoris                   |
| Rio/Рио                | rio                      |
| Sorento/Сорэнто        | sorento                  |
| Soul/Соул              | soul                     |
| Spectra/Спектра        | spectra                  |
| Sportage/Спортэйж      | sportage                 |
| Stinger/Стингер        | stinger                  |
| Venga/Венга            | Venga                    |

\pagebreak

#### <a name="car-models-of-land-car-maker"></a>Car Models of "Land Rover" Car Maker

| Label (human-readable)    | Value (used in requests) |
| -                         | -                        |
| Defender                  | defender                 |
| Discovery                 | discovery                |
| Discovery Sport           | discovery sport          |
| Freelander                | freelander               |
| Range Rover               | range                    |
| Range Rover Autobiography | autobiography            |
| Range Rover Evoque        | rangeEvoque              |
| Range Rover Sport         | rangeSport               |
| Range Rover Velar         | rangeVelar               |
| Range Rover Vogue         | range_rover_vogue        |

\pagebreak

#### <a name="car-models-of-lexus-car-maker"></a>Car Models of "Lexus" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| CT200H                 | ct200h                   |
| ES                     | es                       |
| GS                     | gs                       |
| GX                     | GX                       |
| IS                     | is                       |
| LS                     | ls                       |
| LX                     | LX                       |
| NX                     | nx                       |
| RX                     | RX                       |

\pagebreak

#### <a name="car-models-of-mazda-car-maker"></a>Car Models of "Mazda" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 1                      | mazda1                   |
| 121                    | 121                      |
| 2                      | mazda2                   |
| 3                      | mazda3                   |
| 323                    | 323                      |
| 323C                   | 323c                     |
| 323F                   | 323f                     |
| 5                      | mazda5                   |
| 6                      | mazda6                   |
| 626                    | 626                      |
| 929                    | 929                      |
| Atenza                 | atenza                   |
| Autozam AZ-1           | autozamaz1               |
| Autozam AZ-3           | autozamaz3               |
| Axela                  | axela                    |
| AZ-Offroad             | azoffroad                |
| Biante                 | biante                   |
| Bongo                  | bongo                    |
| Bongo Brawny           | bongobrawny              |
| Bongo Brawny Truck     | bongobrawnytruck         |
| Bongo Friendee         | bongofriendee            |
| Bongo Truck            | bongotruck               |
| B-Series               | mazdab                   |
| BT-50                  | bt50                     |
| Business               | business                 |
| Capella                | capella                  |
| Carol                  | carol                    |
| Clef                   | clef                     |
| Cronos                 | cronos                   |
| CX-3                   | CX-3                     |
| CX-4                   | cx4                      |
| CX-5                   | CX-5                     |
| CX-7                   | CX-7                     |
| CX-8                   | cx8                      |
| CX-9                   | CX-9                     |
| Demio                  | demio                    |
| E 1600                 | e1600                    |
| E 2000                 | e2000                    |
| E 2200                 | e2200                    |
| Etude                  | etude                    |
| Eunos                  | eunos                    |
| Eunos 100              | eunos100                 |
| Eunos 300              | eunos300                 |
| Eunos 500              | eunos500                 |
| Eunos 800              | eunos800                 |
| Eunos Cargo            | eunoscargo               |
| Eunos Cosmo            | eunoscosmo               |
| Eunos Presso           | eunospresso              |
| Eunos Roadster         | eunosroadster            |
| Familia                | familia                  |
| Familia S-Wagon        | familiaswagon            |
| Flair                  | flair                    |
| Flair Crossover        | flaircrossover           |
| Flairwagon             | flairwagon               |
| Lantis                 | lantis                   |
| Laputa                 | laputa                   |
| Levante                | levante                  |
| Luce                   | luce                     |
| Marvie                 | marvie                   |
| Millenia               | millenia                 |
| MPV                    | mpv                      |
| MS-6                   | ms6                      |
| MS-8                   | ms8                      |
| MS-9                   | ms9                      |
| MX-3                   | mx3                      |
| MX-5                   | MX-5                     |
| MX-6                   | mx6                      |
| Navajo                 | navajo                   |
| Persona                | persona                  |
| Premacy                | premacy                  |
| Proceed                | proceed                  |
| Protege                | protege                  |
| Protege5               | protege5                 |
| Revue                  | revue                    |
| Rustler                | rustler                  |
| RX-7                   | rx7                      |
| RX-8                   | RX-8                     |
| Scrum                  | scrum                    |
| Sentia                 | sentia                   |
| Spiano                 | spiano                   |
| Titan                  | titan                    |
| Tribute                | Tribute                  |
| Truck                  | truck                    |
| Unnamed                | unnamed                  |
| Vantrend               | vantrend                 |
| Verisa                 | verisa                   |
| Xedos 6                | xedos6                   |
| Xedos 9                | xedos9                   |

\pagebreak

#### <a name="car-models-of-mercedes-car-maker"></a>Car Models of "Mercedes-Benz" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| A                      | aClass                   |
| B                      | bClass                   |
| C                      | cClass                   |
| CL                     | cl                       |
| CLA                    | CLA                      |
| CLC                    | CLC                      |
| CLS                    | cls                      |
| E                      | eClass                   |
| G                      | G                        |
| GL                     | GL                       |
| GLA                    | gla                      |
| GLC                    | glc                      |
| GLE                    | gle                      |
| GLK                    | GLK                      |
| GLS                    | gls                      |
| ML                     | ML                       |
| R                      | R                        |
| S                      | sClass                   |
| SLK                    | slk                      |
| Sprinter               | Sprinter                 |
| V                      | vclass                   |
| Viano                  | Viano                    |
| Vito                   | Vito                     |

\pagebreak

#### <a name="car-models-of-mini-car-maker"></a>Car Models of "MINI" Car Maker

| Label (human-readable)               | Value (used in requests) |
| -                                    | -                        |
| Clubman / Клабмэн                    | clubman                  |
| Cooper D / Купер Д                   | Cooper D                 |
| Cooper SD                            | cooper_sd                |
| Cooper S / Купер Эс                  | coopers                  |
| Cooper / Купер                       | Cooper                   |
| Countryman / Кантримэн               | countryman               |
| John Cooper Works / Джон Купер Воркс | johnworks                |
| One / Уан                            | One                      |
| Paceman / Пэйсмэн                    | paceman                  |

\pagebreak

#### <a name="car-models-of-mitsubishi-car-maker"></a>Car Models of "Mitsubishi" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| ASX                    | ASX                      |
| Carisma                | Carizma                  |
| Colt                   | colt                     |
| Fuso                   | Fuso                     |
| Galant                 | galant                   |
| Grandis                | grandis                  |
| L200                   | L200                     |
| Lancer                 | lancer                   |
| Montero                | montero                  |
| Outlander              | Outlander                |
| Outlander XL           | Outlander XL             |
| Pajero                 | Pajero                   |
| Pajero Sport           | Pajero Sport             |
| Proudia                | proudia                  |

\pagebreak

#### <a name="car-models-of-nissan-car-maker"></a>Car Models of "Nissan" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 100NX                  | 100nx                    |
| 180SX                  | 180sx                    |
| 200SX                  | 200sx                    |
| 240SX                  | 240sx                    |
| 300ZX                  | 300zx                    |
| 350Z                   | 350z                     |
| 370Z                   | 370z                     |
| AD                     | ad                       |
| Almera                 | almera                   |
| Almera Classic         | almeraclassic            |
| Altima                 | altima                   |
| Armada                 | armada                   |
| Auster                 | auster                   |
| Avenir                 | avenir                   |
| Avenir Salut           | avenirsalut              |
| Bassara                | bassara                  |
| BE-1                   | be1                      |
| Bluebird               | bluebird                 |
| Bluebird Maxima        | bluebirdmaxima           |
| Bluebird Sylphy        | bluebirdsylphy           |
| Cabstar                | cabstar                  |
| Caravan                | caravan                  |
| Caravan Elgrand        | caravanelgrand           |
| Cedric                 | cedric                   |
| Cedric Cima            | cedriccima               |
| Cefiro                 | cefiro                   |
| Cherry                 | cherry                   |
| Cima                   | cima                     |
| Civilian               | civilian                 |
| Clipper                | clipper                  |
| Condor                 | condor                   |
| Crew                   | crew                     |
| Cube                   | cube                     |
| Cube Cubic             | cubecubic                |
| Datsun                 | datsun                   |
| DAYZ                   | dayz                     |
| DAYZ Roox              | dayzroox                 |
| Diesel                 | diesel                   |
| Dualis                 | dualis                   |
| Elgrande               | elgrande                 |
| e-NV                   | env                      |
| Exa                    | exa                      |
| Fairlady Z             | fairladyz                |
| Figaro                 | figaro                   |
| Frontier               | frontier                 |
| Fuga                   | fuga                     |
| Gloria                 | gloria                   |
| Gloria Cima            | gloriacima               |
| Grand Livina           | grandlivina              |
| GT-R                   | gtr                      |
| Hino                   | hino                     |
| Homy                   | homy                     |
| Homy Elgrand           | homyelgrand              |
| Hypermini              | hypermini                |
| Juke                   | juke                     |
| King Cab               | kingcab                  |
| Kix                    | kix                      |
| Lafesta                | lafesta                  |
| Langley                | langley                  |
| Largo                  | largo                    |
| Latio                  | latio                    |
| Laurel                 | laurel                   |
| Laurel Spirit          | laurelspirit             |
| Leaf                   | leaf                     |
| Leopard                | leopard                  |
| Liberta Villa          | libertavilla             |
| Liberty                | Liberty                  |
| Livina                 | livina                   |
| Lucino                 | lucino                   |
| March                  | March                    |
| March Box              | marchbox                 |
| Maxima                 | maxima                   |
| Micra                  | micra                    |
| Micra C+C              | micracc                  |
| Mistral                | mistral                  |
| Moco                   | moco                     |
| Murano                 | murano                   |
| Navara                 | Navara                   |
| Note                   | note                     |
| NP300                  | np300                    |
| NT100 Clipper          | nt100clipper             |
| NV100 Clipper          | nv100clipper             |
| NV150 AD               | nv150ad                  |
| NV200                  | nv200                    |
| NV350 Caravan          | nv350caravan             |
| NX-Coupe               | nxcoupe                  |
| Otti                   | otti                     |
| Pao                    | pao                      |
| Pathfinder             | pathfinder               |
| Patrol                 | Patrol                   |
| Pino                   | pino                     |
| Pixo                   | pixo                     |
| Prairie                | Prairie                  |
| Prairie Joy            | prairiejoy               |
| Presage                | presage                  |
| Presea                 | presea                   |
| President              | president                |
| Primastar              | primastar                |
| Primera                | primera                  |
| Primera Camino         | primeracamino            |
| Pulsar                 | pulsar                   |
| Qashqai                | qashqai                  |
| Qashqai+2              | qashqai2                 |
| Quest                  | quest                    |
| Rasheen                | rasheen                  |
| Rnessa                 | rnessa                   |
| Rogue                  | rogue                    |
| Roox                   | roox                     |
| Safari                 | safari                   |
| S-Cargo                | scargo                   |
| Sentra                 | sentra                   |
| Serena                 | serena                   |
| Silvia                 | silvia                   |
| Skyline                | skyline                  |
| Skyline Crossover      | skylinecrossover         |
| Skyline GT-R           | skylinegtr               |
| Stagea                 | stagea                   |
| Stanza                 | stanza                   |
| Sunny                  | sunny                    |
| Sunny California       | sunnycalifornia          |
| Sunny RZ-1             | sunnyrz1                 |
| Sylphy                 | sylphy                   |
| Teana                  | teana                    |
| Terrano                | terrano                  |
| Terrano II             | terranoii                |
| Terrano Regulus        | terranoregulus           |
| Tiida                  | tiida                    |
| Tiida Latio            | tiidalatio               |
| Tino                   | tino                     |
| Urvan                  | urvan                    |
| Vanette                | vanette                  |
| Vanette Serena         | vanetteserena            |
| Vanette Truck          | vanettetruck             |
| Versa                  | versa                    |
| Wingroad               | wingroad                 |
| Xterra                 | xterra                   |
| X-Trail                | xTrail                   |

\pagebreak

#### <a name="car-models-of-opel-car-maker"></a>Car Models of "Opel" Car Maker

| Label (human-readable)    | Value (used in requests) |
| -                         | -                        |
| Adam/Адам                 | adam                     |
| Agila                     | agila                    |
| Antara                    | antara                   |
| Astra GTC/Астра Джи Ти Си | astragtc                 |
| Astra/Астра               | astra                    |
| Calibra/Калибра           | calibra                  |
| Combo                     | combo                    |
| Corsa/Корса               | corsa                    |
| Frontera/Фронтэра         | frontera                 |
| Insignia/Инсигния         | insignia                 |
| Kadett                    | kadett                   |
| Meriva/Мерива             | meriva                   |
| Mokka/Мокка               | mokka                    |
| Monterey                  | monterey                 |
| Movano                    | movano                   |
| Omega/Омега               | omega                    |
| Signum                    | signum                   |
| Sintra                    | sintra                   |
| Tigra                     | tigra                    |
| Vectra/Вектра             | vectra                   |
| Vita                      | vita                     |
| Vivaro                    | vivaro                   |
| Zafira/Зафира             | zafira                   |

\pagebreak

#### <a name="car-models-of-peugeot-car-maker"></a>Car Models of "Peugeot" Car Maker

| Label (human-readable)      | Value (used in requests) |
| -                           | -                        |
| 1007                        | 1007                     |
| 106                         | 106                      |
| 107                         | 107                      |
| 2008                        | 2008                     |
| 206                         | 206                      |
| 206+                        | 206+                     |
| 207                         | 207                      |
| 207CC                       | 207CC                    |
| 208                         | 208                      |
| 3008                        | 3008                     |
| 301                         | 301                      |
| 307                         | 307                      |
| 308                         | 308                      |
| 308CC                       | 308CC                    |
| 309                         | 309                      |
| 4007                        | 4007                     |
| 4008                        | 4008                     |
| 407                         | 407                      |
| 408                         | 408                      |
| 5008                        | 5008                     |
| 508                         | 508                      |
| 607                         | 607                      |
| 807                         | 807                      |
| Bipper                      | bipper                   |
| Boxer/Боксер (коммерческий) | boxer                    |
| Expert                      | expert                   |
| Ion                         | ion                      |
| Partner/Партнёр             | partner                  |
| RCZ                         | rcz                      |
| Traveller                   | traveller                |

\pagebreak

#### <a name="car-models-of-renault-car-maker"></a>Car Models of "Renault" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Clio                   | clio                     |
| Dokker                 | dokker                   |
| Duster                 | Duster                   |
| Fluence                | Fluence                  |
| Kangoo                 | Kangoo                   |
| Kaptur                 | Kaptur                   |
| Koleos                 | Koleos                   |
| Laguna                 | laguna                   |
| Latitude               | Latitude                 |
| Logan                  | logan                    |
| Master                 | master                   |
| Megane                 | megane                   |
| Sandero                | Sandero                  |
| Scenic                 | Scenic                   |
| SR                     | SR                       |
| Symbol                 | symbol                   |
| Trafic                 | trafic                   |
| Twingo                 | twingo                   |
| Vel Satis              | velSatis                 |

\pagebreak

#### <a name="car-models-of-saab-car-maker"></a>Car Models of "Saab" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 9000                   | 9000                     |
| 9-3                    | saab93                   |

\pagebreak

#### <a name="car-models-of-seat-car-maker"></a>Car Models of "Seat" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Altea                  | altea                    |
| Cordoba                | Cordoba                  |
| Ibiza                  | Ibiza                    |
| Leon                   | leon                     |
| Toledo                 | toledo                   |

\pagebreak

#### <a name="car-models-of-skoda-car-maker"></a>Car Models of "Skoda" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Fabia                  | fabia                    |
| Felicia                | Felicia                  |
| Karoq                  | Karoq                    |
| Kodiaq                 | kodiaq                   |
| Octavia                | octavia                  |
| Rapid                  | rapid                    |
| Roomster               | Roomster                 |
| Superb                 | superb                   |
| Yeti                   | Yeti                     |

\pagebreak

#### <a name="car-models-of-smart-car-maker"></a>Car Models of "Smart" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Fortwo                 | fortwo                   |

\pagebreak

#### <a name="car-models-of-subaru-car-maker"></a>Car Models of "Subaru" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 1300                   | 1300                     |
| 1600                   | 1600                     |
| 1800                   | 1800                     |
| Alcyone                | alcyone                  |
| Ascent                 | ascent                   |
| B9 Tribeca             | b9tribeca                |
| Baja                   | baja                     |
| Bistro                 | bistro                   |
| Brat                   | brat                     |
| BRZ                    | brz                      |
| Chiffon                | chiffon                  |
| Crosstrek              | crosstrek                |
| Dex                    | dex                      |
| Dias Wagon             | diaswagon                |
| Domingo                | domingo                  |
| Exiga                  | exiga                    |
| Exiga Crossover        | exigacrossover           |
| Forester               | Forester                 |
| Impreza                | impreza                  |
| Impreza WRX            | imprezawrx               |
| Impreza WRX STI        | imprezawrxsti            |
| Impreza XV             | imprezaxv                |
| Justy                  | justy                    |
| Legacy                 | legacy                   |
| Legacy B4              | legacyb4                 |
| Legacy Lancaster       | legacylancaster          |
| Leone                  | leone                    |
| Levorg                 | levorg                   |
| Libero                 | libero                   |
| Lucra                  | lucra                    |
| Outback                | Outback                  |
| Pleo                   | pleo                     |
| Pleo Nesta             | pleonesta                |
| Pleo Plus              | pleoplus                 |
| R1                     | r1                       |
| Rex                    | rex                      |
| Sambar                 | sambar                   |
| Sambar Truck           | sambartruck              |
| Stella/Стэлла          | stella                   |
| SVX                    | svx                      |
| Traviq                 | traviq                   |
| Trezia                 | trezia                   |
| Tribeca                | tribeca                  |
| Vivio                  | vivio                    |
| XT                     | xt                       |
| XV                     | xv                       |

\pagebreak

#### <a name="car-models-of-suzuki-car-maker"></a>Car Models of "Suzuki" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| Aerio                  | aerio                    |
| Alto                   | alto                     |
| Alto Lapin             | altolapin                |
| Baleno                 | baleno                   |
| Cappuccino             | cappuccino               |
| Cara                   | cara                     |
| Carry Truck            | carrytruck               |
| Carry Van              | carryvan                 |
| Celerio                | celerio                  |
| Cervo                  | cervo                    |
| Cultus                 | cultus                   |
| Cultus Crescent        | cultuscrescent           |
| Equator                | equator                  |
| Escudo                 | escudo                   |
| Esteem                 | esteem                   |
| Every                  | every                    |
| Forenza                | forenza                  |
| Fronte                 | fronte                   |
| Grand Escudo           | grandescudo              |
| Grand Vitara           | Grand Vitara             |
| Grand Vitara XL-7      | grandvitaraxl7           |
| Hustler                | hustler                  |
| Ignis                  | ignis                    |
| Jimny                  | Jimny                    |
| Jimny Sierra           | jimnysierra              |
| Jimny Wide             | jimnywide                |
| Join                   | join                     |
| Joy Pop                | joypop                   |
| Kei                    | kei                      |
| Kizashi                | Kizashi                  |
| Landy                  | landy                    |
| Lapin                  | lapin                    |
| Liana                  | liana                    |
| MR Wagon               | mrwagon                  |
| Palette                | palette                  |
| Samurai                | samurai                  |
| Sidekick               | sidekick                 |
| Solio                  | solio                    |
| Spacia                 | spacia                   |
| Splash                 | Splash                   |
| Swift                  | swift                    |
| SX4                    | sx4                      |
| Twin                   | twin                     |
| Verona                 | verona                   |
| Vitara                 | vitara                   |
| Wagon R                | wagonr                   |
| Wagon R Plus           | wagonrplus               |
| Wagon R Solio          | wagonrsolio              |
| Wagon R Wide           | wagonrwide               |
| Works                  | works                    |
| X-9                    | x9                       |
| Xbee                   | xbee                     |
| XL7                    | xl7                      |

\pagebreak

#### <a name="car-models-of-toyota-car-maker"></a>Car Models of "Toyota" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 4Runner                | 4runner                  |
| Allex                  | allex                    |
| Allion                 | allion                   |
| Alphard                | Alphard                  |
| Altezza                | altezza                  |
| Aqua                   | aqua                     |
| Aristo                 | aristo                   |
| Aurion                 | aurion                   |
| Auris                  | auris                    |
| Avalon                 | avalon                   |
| Avensis                | avensis                  |
| Avensis Verso          | avensis_verso            |
| Aygo                   | aygo                     |
| bB                     | bb                       |
| Belta                  | belta                    |
| Blade                  | blade                    |
| Blizzard               | blizzard                 |
| Brevis                 | brevis                   |
| Caldina                | Caldina                  |
| Cami                   | cami                     |
| Camry                  | camry                    |
| Camry Gracia           | camrygracia              |
| Camry Prominent        | camryprominent           |
| Carina                 | carina                   |
| Carina E               | carinae                  |
| Carina ED              | carinaed                 |
| Carina II              | carinaii                 |
| Celica                 | celica                   |
| Celsior                | celsior                  |
| Century                | century                  |
| Chaser                 | chaser                   |
| C-HR                   | chr                      |
| Classic                | classic                  |
| Coaster                | coaster                  |
| Comfort                | comfort                  |
| Corolla                | corolla                  |
| Corolla Axio           | corollaaxio              |
| Corolla Ceres          | corollaceres             |
| Corolla Fielder        | corollafielder           |
| Corolla FX             | corollafx                |
| Corolla II             | corollaii                |
| Corolla Levin          | corollalevin             |
| Corolla Rumion         | corollarumion            |
| Corolla Runx           | corollarunx              |
| Corolla Spacio         | corollaspacio            |
| Corolla Verso          | corollaverso             |
| Corona                 | Corona                   |
| Corona Exiv            | coronaexiv               |
| Corona Premio          | coronapremio             |
| Corona SF              | coronasf                 |
| Corsa                  | corsa                    |
| Cressida               | cressida                 |
| Cresta                 | cresta                   |
| Crown                  | crown                    |
| Crown Majesta          | crownmajesta             |
| Curren                 | curren                   |
| Cynos                  | cynos                    |
| Deliboy                | deliboy                  |
| Duet                   | duet                     |
| Dyna                   | dyna                     |
| Echo                   | echo                     |
| Esquire                | esquire                  |
| Estima                 | estima                   |
| Estima Emina           | estimaemina              |
| Estima Lucida          | estimalucida             |
| Etios                  | etios                    |
| FJ Cruiser             | fjcruiser                |
| Fortuner               | fortuner                 |
| Funcargo               | funcargo                 |
| Gaia                   | Gaia                     |
| Grand Hiace            | grandhiace               |
| Granvia                | granvia                  |
| GT                     | gt                       |
| GT86                   | gt86                     |
| Harrier                | Harrier                  |
| Hiace                  | hiace                    |
| Hiace Regius           | hiaceregius              |
| Hiace Truck            | hiacetruck               |
| Highlander             | Highlander               |
| Hilux                  | Hilux                    |
| Hilux Pick Up          | hiluxpickup              |
| Hilux Surf             | hiluxsurf                |
| Innova                 | innova                   |
| Ipsum                  | Ipsum                    |
| iQ                     | iq                       |
| Isis                   | isis                     |
| Ist                    | ist                      |
| JPN TAXI               | jpntaxi                  |
| Kluger V               | klugerv                  |
| Land Cruiser           | landCruiser              |
| Land Cruiser 100       | landCruiser100           |
| Land Cruiser 200       | Land Cruiser 200         |
| Land Cruiser Cygnus    | landcruisercygnus        |
| Land Cruiser Prado     | prado                    |
| Lite Ace               | liteace                  |
| Lite Ace Noah          | liteacenoah              |
| Lite Ace Truck         | liteacetruck             |
| Mark II                | mark                     |
| Mark II Wagon Blit     | markiiwagonblit          |
| Mark II Wagon Qualis   | markiiwagonqualis        |
| Mark X                 | markx                    |
| Mark X Zio             | markxzio                 |
| Master Ace Surf        | masteracesurf            |
| Mega Cruiser           | megacruiser              |
| Mirai                  | mirai                    |
| MR2                    | mr2                      |
| MR-S                   | mrs                      |
| Nadia                  | nadia                    |
| Noah                   | noah                     |
| Opa                    | opa                      |
| Origin                 | origin                   |
| Paseo                  | paseo                    |
| Passo                  | passo                    |
| Passo Sette            | passosette               |
| Picnic                 | picnic                   |
| Pixis Epoch            | pixisepoch               |
| Pixis Joy              | pixisjoy                 |
| Pixis Mega             | pixismega                |
| Pixis Space            | pixisspace               |
| Pixis Truck            | pixistruck               |
| Pixis Van              | pixisvan                 |
| Platz                  | platz                    |
| Porte                  | porte                    |
| Premio                 | premio                   |
| Previa                 | previa                   |
| Prius                  | prius                    |
| Prius a                | priusa                   |
| Prius C                | priusc                   |
| Prius PHV              | priusphv                 |
| Prius Prime            | priusprime               |
| Prius v                | priusv                   |
| Proace                 | proace                   |
| Probox                 | probox                   |
| Progres                | progres                  |
| Pronard                | pronard                  |
| Ractis                 | ractis                   |
| Raum                   | raum                     |
| RAV-4                  | rav4                     |
| Regius                 | regius                   |
| Regius Ace             | regiusace                |
| Roomy                  | roomy                    |
| Rush                   | rush                     |
| Sai                    | sai                      |
| Scepter                | scepter                  |
| Sequoia                | Sequoia                  |
| Sera                   | sera                     |
| Sienna                 | sienna                   |
| Sienta                 | sienta                   |
| Soarer                 | soarer                   |
| Solara                 | solara                   |
| Soluna                 | soluna                   |
| Spade                  | spade                    |
| Sparky                 | sparky                   |
| Sprinter Carib         | sprintercarib            |
| Sprinter Marino        | sprintermarino           |
| Sprinter Trueno        | sprintertrueno           |
| Starlet                | starlet                  |
| Succeed                | succeed                  |
| Supra                  | supra                    |
| Tacoma                 | tacoma                   |
| Tank                   | tank                     |
| Tercel                 | tercel                   |
| Touring Hiace          | touringhiace             |
| Town Ace               | townace                  |
| Town Ace Noah          | townacenoah              |
| Town Ace Truck         | townacetruck             |
| ToyoAce                | toyoace                  |
| Tundra                 | tundra                   |
| Urban Cruiser          | urbancruiser             |
| Vanguard               | vanguard                 |
| Vellfire               | vellfire                 |
| Venza                  | Venza                    |
| Verossa                | verossa                  |
| Verso                  | verso                    |
| Verso-s                | versos                   |
| Vios                   | vios                     |
| Vista                  | Vista                    |
| Vista Ardeo            | vistaardeo               |
| Vitz                   | vitz                     |
| Voltz                  | voltz                    |
| Voxy                   | voxy                     |
| WiLL Cypha             | willcypha                |
| WiLL Vi                | willvi                   |
| WiLL VS                | willvs                   |
| Windom                 | windom                   |
| Wish                   | wish                     |
| Yaris                  | yaris                    |

\pagebreak

#### <a name="car-models-of-vw-car-maker"></a>Car Models of "Volkswagen" Car Maker

| Label (human-readable)                      | Value (used in requests) |
| -                                           | -                        |
| Amarok/Амарок (коммерческий)                | amarok                   |
| Atlas                                       | atlas                    |
| Beetle/Битл                                 | Beetle                   |
| Bora/Бора                                   | Bora                     |
| Caddy/Кэдди (коммерческий)                  | caddy                    |
| California /Калифорния (коммерческий)       | calif                    |
| Caravelle/Каравэлла (коммерческий)          | caravelle                |
| Crafter/Крафтэр (коммерческий)              | crafter                  |
| Eos/Эос                                     | eos                      |
| Golf Plus/Гольф Плюс                        | golfPlus                 |
| Golf/Гольф                                  | golf                     |
| Jetta/Джетта                                | jetta                    |
| Lupo/Лупо                                   | lupo                     |
| Multivan/Мультивэн (коммерческий)           | multivan                 |
| Passat CC/Пассат ЦЦ                         | passatCC                 |
| Passat/Пассат                               | passat                   |
| Phaeton/Фаэтон                              | phaeton                  |
| Pointer/Поинтер                             | pointer                  |
| Polo/Поло                                   | polo                     |
| Scirocco /Сирокко                           | scirocco                 |
| Scirocco/Сирокко                            | scirocco                 |
| Sharan/Шаран                                | sharan                   |
| T5/Транспортер (коммерческий)               | t5                       |
| T6 /Тэ 6                                    | T6                       |
| Teramont                                    | teramont                 |
| Tiguan/Тигуан                               | tiguan                   |
| Touareg/Туарег                              | touareg                  |
| Touran/Туран                                | touran                   |
| Transporter/Транспортер (Т5) (коммерческий) | transporter              |
| T-ROC                                       | T-ROC                    |
| Vento                                       | vento                    |

\pagebreak

#### <a name="car-models-of-volvo-car-maker"></a>Car Models of "Volvo" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| C30                    | c30                      |
| S40                    | s40                      |
| S60                    | s60                      |
| S80                    | s80                      |
| S90                    | s90                      |
| V40                    | v40                      |
| V50                    | v50                      |
| V60                    | V60                      |
| V70                    | V70                      |
| V90                    | v90                      |
| XC40                   | xc40                     |
| XC60                   | XC60                     |
| XC70                   | XC70                     |
| XC90                   | XC90                     |

\pagebreak

#### <a name="car-models-of-lada-car-maker"></a>Car Models of "ВАЗ (Lada)" Car Maker

| Label (human-readable) | Value (used in requests) |
| -                      | -                        |
| 1111 (Oka)             | 1111                     |
| 2104                   | 2104                     |
| 21041                  | 21041                    |
| 21043                  | 21043                    |
| 2105                   | 2105                     |
| 2107                   | 2107                     |
| 2108                   | 2108                     |
| 2109                   | 2109                     |
| 2110                   | 2110                     |
| 2112                   | 2112                     |
| 2113 (Samara)          | 2113Samara               |
| 2114 (Samara)          | 2114Samara               |
| 2115 (Samara)          | 2115Samara               |
| 2121 (Niva)            | 2121Niva                 |
| 2131                   | 2131                     |
| Granta                 | granta                   |
| Kalina                 | kalina                   |
| Largus                 | largus                   |
| Priora                 | priora                   |
| Vesta                  | vesta                    |
| X-Ray                  | xray                     |
[//]: # (#CAR_MODELS_END# -- separator for automation scripts)

## Notes

<a name="note-1">¹</a> For compatibility instead of an integer number it's
allowed to pass a string `"ramc2"` or `"Cadillac"`.

<a name="note-2">²</a> See [Car Makers](#car-makers-dict) dictionary
for possible values.

<a name="note-3">³</a> See [Car Models](#car-models) dictionaries
for possible values.
