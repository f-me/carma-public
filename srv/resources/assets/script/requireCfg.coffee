require.config
  baseUrl: "/s/js/gen"
  paths:
    jquery  : "/s/3p/jquery"
    text    : "/s/3p/rjs/text"
    domready: "/s/3p/rjs/domReady"
    json    : "/s/3p/rjs/json"
    tpl     : "/s/tpl"
    finch   : "/s/3p/finch.min"
    d3      : "/s/3p/d3.min"
    knockout: "/s/3p/knockout"
    base64  : "/s/3p/base64.min"
    bootstrap: '/s/3p/bootstrap/js/bootstrap'
    wysihtml5: "/s/3p/wysihtml5/bootstrap3-wysihtml5.all.min"
    'wysihtml5-ru': "/s/3p/wysihtml5/locales/bootstrap-wysihtml5.ru-RU"
    datatables : '/s/3p/datatables/js/jquery.datatables.min'
  shim:
    shim:
      bootstrap:
        deps: ['jquery']
    finch:
      exports: "Finch"
    d3:
      exports: "d3"
    base64:
      exports: "Base64"
  waitSeconds: 15
