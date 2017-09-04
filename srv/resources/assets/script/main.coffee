require "carma/vendor"
require "carma/globallibs"
require "carma-styles"
vinTpl = require "carma-tpl/screens/vin.pug"
tpl = require "carma-tpl/partials/partnersSearch.pug"

console.info "greetings!", ThMenu
console.info "vinTpl", vinTpl
console.info "tpl", tpl()
