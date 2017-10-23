# TaskManager client-side helpers
#
# Example:
#
# bvm = newTaskVM ()
# ko.applyBindings vm, someElement
# $.ajax(url: "/createSomeTask" ... ).done((res) -> handleTask res.token, bvm)
#
# Now use bvm fields to access task data

{$, _, ko} = require "carma/vendor"

# Create a new ViewModel for tracking task status
newTaskVM = () ->
  token     : ko.observable null
  done      : ko.observable false
  errorMsg  : ko.observable null
  resultMsg : ko.observable null
  fileUrls  : ko.observableArray()
  cleanup   : () ->
    if @token()?
      $.ajax
        type: "DELETE"
        url:  "/tasks/#{@token()}"

# Watch task status and update task VM as it changes.
# resultFormatter and errorFormatter are applied to msg served in
# JSON of finished and failed tasks.
handleTaskWith = (token, bvm, resultFormatter, errorFormatter) ->
  bvm.token token
  $.getJSON("/tasks/#{token}/status").
    done((res) ->
      switch res.status
        when "inprogress"
          bvm.done false
          setTimeout (() ->
            handleTaskWith(token, bvm, resultFormatter, errorFormatter)),
            1000
        when "finished"
          bvm.done true
          if res.files?.length > 0
            for f in res.files
              bvm.fileUrls.push "/tasks/#{token}/files/#{f}"
          bvm.resultMsg resultFormatter res.msg
        when "failed"
          bvm.done true
          bvm.errorMsg errorFormatter res.msg)

handleTask = (token, bvm) ->
  handleTaskWith token, bvm, _.identity, _.identity

module.exports = {
  handleTask
  handleTaskWith
  newTaskVM
}
