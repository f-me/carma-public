{ko} = require "carma/vendor"
template = require "carma-tpl/screens/diagTree/show.pug"

constructor = (viewName, args) ->
  viewModel = caseId: ko.observable args.caseId
  ko.applyBindings viewModel, document.getElementById("diag-tree-show-screen")

module.exports = {template, constructor}
