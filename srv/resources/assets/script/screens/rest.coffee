define ["screens/rest.jade"], (tpl) ->

  constructor: ->
    um = window.global.Usermeta
    ko.applyBindings(um, $("#display-current-user")[0])
  template: tpl()
