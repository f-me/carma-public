{ko} = require "carma/vendor"
template = require "carma-tpl/screens/diagTree/editor.pug"

constructor = (viewName, args) ->
  ko.applyBindings {}, document.getElementById("diag-tree-editor-screen")

module.exports = {template, constructor}
