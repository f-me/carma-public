require.config
  baseUrl: "/s/js/gen"
  paths:
    text    : "/s/3p/rjs/text"
    domready: "/s/3p/rjs/domReady"
    json    : "/s/3p/rjs/json"
    tpl     : "/s/tpl"
    finch   : "/s/3p/finch.min"
    d3      : "/s/3p/d3.min"
  shim:
    finch:
      exports: "Finch"
    d3:
      exports: "d3"
  waitSeconds: 15
