{$, ko} = require "carma/vendor"
template = require "carma-tpl/screens/rest.pug"

module.exports = {
  constructor: ->
    um = window.global.Usermeta
    ko.applyBindings(um, $("#display-current-user")[0])
  template
}
