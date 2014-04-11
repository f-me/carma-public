require.config
  baseUrl: "/s/js/gen"
  paths:
    text    : "/s/js/3p/rjs/text"
    domready: "/s/js/3p/rjs/domReady"
    json    : "/s/js/3p/rjs/json"
    tpl     : "/s/tpl"
    finch   : "/s/js/3p/finch.min"
  shim:
    finch:
      exports: "Finch"
  waitSeconds: 15
