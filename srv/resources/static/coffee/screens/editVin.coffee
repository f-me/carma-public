define ["model/main", "text!tpl/screens/editVin.html"], (main, tpl) ->
  setupEditVin = (viewName, args)->
    main.modelSetup("vin") viewName, args,
                       permEl       : "vin-permissions"
                       focusClass   : "focusable"
                       slotsee      : []
                       groupsForest : "center"
                       fetchCb      : ->

  doNewVin = (e) ->
    e.preventDefault()
    formId = $('#new-vin').find('input[name=id]').val().toUpperCase()
    vinRe = new RegExp global.dictLabelCache["_regexps"]["vin"]
    unless vinRe.test formId
      return unless confirm "Введенный VIN не проходит проверку корректности,
        продолжить?"
    $.ajax
      type: "POST"
      url: "/_/findOrCreate/vin/#{formId}"
      data:
        JSON.stringify
        # we set vin = id here because, we can create model
        # with specified id, but vin is unusual model and it's
        # id field called vin, also vin it's actual redis key
          vin:       formId
          callTaker: global.user.meta.realName
      success: (args) ->
        console.log args
        global.router.navigate("editVin/#{args.id}")
        renderScreen("editVin", args)
      error: (e) ->
        console.log e
  { constructor: setupEditVin
  , template: tpl
  }
