{ko: {observable, observableArray}} = require "carma/vendor"
{tpl} = require "carma/globallibs"
{data} = require "carma/data"

smsTemplates = data.model.SmsTemplate.filter (x) => x.isActive

class SmsFormViewModel
  constructor: ({@valuesModel, @closeForm})->
    @smsTemplates = observableArray smsTemplates

  dispose: =>

module.exports =
  template:  tpl require "carma-tpl/neoComponents/SmsForm.pug"
  viewModel: SmsFormViewModel
