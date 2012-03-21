{
    "name": "sober",
    "title": "Трезвый водитель",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "fromAddress",
            "meta": {
                "label": "Где забрать"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "toAddress",
            "meta": {
                "label": "Куда доставить"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "multidrive",
            "meta": {
                "label": "Мультидрайв"
            },
            "type": "checkbox",
            "canWrite": true,
            "canRead": true
        }
    ]
}
