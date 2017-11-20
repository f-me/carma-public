{_, ko} = require "carma/vendor"


# for f in fields when f.meta.reactComponent
#   do (f) ->
#     kvm._reactComponents.push
#       id: "#{kvm._meta.cid}-#{f.name}"
#       component: React.createElement(
#         CarmaComponents[f.meta.reactComponent],
#         value: kvm[f.name]()
#         onChange: (o) -> kvm[f.name](_.clone o)
#         kvm: kvm)
class AvarcomTasksViewModel
  constructor: ->
  dispose: =>


module.exports =
  componentName: "avarcom-tasks"

  component:
    template:  require "./template.pug"
    viewModel: AvarcomTasksViewModel
