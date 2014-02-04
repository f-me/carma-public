# TaskManager client-side helpers
#
# Example:
#
# vm = newTaskVM ()
# ko.applyBindings vm, someElement
# $.ajax(url: "/createSomeTask" ... ).done((res) -> handleTask res.token, bvm)

define ->
  # Create a new ViewModel for tracking task status
  newTaskVM = () ->
    done      : ko.observable false
    errorMsg  : ko.observable null
    resultMsg : ko.observable null
    fileUrls  : ko.observableArray()

  # Watch task status and update task VM as it changes.
  # resultFormatter and errorFormatter are applied to msg served in
  # JSON of finished and failed tasks.
  handleTaskWith = (token, bvm, resultFormatter, errorFormatter) ->
    $.getJSON("/tasks/status/#{token}").
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
                bvm.fileUrls.push "/tasks/getFile/#{token}/#{f}"
            bvm.resultMsg resultFormatter res.msg
          when "failed"
            bvm.done true
            bvm.errorMsg errorFormatter res.msg)

  handleTask = (token, bvm) ->
    handleTaskWith token, bvm, _.identity, _.identity

  { handleTask:     handleTask
  , handleTaskWith: handleTaskWith
  , newTaskVM:      newTaskVM
  }
