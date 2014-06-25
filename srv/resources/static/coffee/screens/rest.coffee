define ["text!tpl/screens/rest.html"], (tpl) ->

  constructor: ->
    um = window.global.Usermeta
    ko.applyBindings(um, $("#display-current-user")[0])
  template: tpl
