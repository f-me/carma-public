this.setupEditVin = (viewName, args)->
  modelSetup("vin") viewName, args,
                     permEl       : "vin-permissions"
                     focusClass   : "focusable"
                     slotsee      : []
                     groupsForest : "center"
                     fetchCb      : ->


this.doNewVin = (e) ->
  e.preventDefault()
  $.ajax
    type: "POST"
    url: "/_/findOrCreate/vin"
    data: $("#new-vin").serialize()
    success: (args) ->
      console.log args
      global.router.navigate("editVin")
      renderScreen("editVin", args)

