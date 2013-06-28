{
  "fields": [
    {
      "groupName": "car",
      "canWrite": [
        "front",
        "back",
        "head",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "parguy",
        "account"
      ],
      "name": "car"
    },
    {
      "groupName": "cardNumber",
      "canWrite": [
        "front",
        "back",
        "head",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "parguy",
        "account"
      ],
      "name": "cardNumber"
    },
    {
      "meta": {
        "infoText": "ownerName",
        "label": "Владелец"
      },
      "groupName": "carContact",
      "canWrite": [
        "front",
        "back",
        "head",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "parguy",
        "account"
      ],
      "name": "contact"
    },
    {
      "meta": {
        "infoText": "program",
        "targetCategory": "program"
      },
      "canWrite": [
        "front",
        "back",
        "head",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "parguy",
        "account"
      ],
      "name": "program"
    }
  ],
  "applications": [
    {
      "canRead": true,
      "canWrite": true,
      "targets": true
    }
  ],
  "canDelete": false,
  "canUpdate": true,
  "canRead": true,
  "canCreate": false,
  "title": "VIN",
  "name": "vin"
}
