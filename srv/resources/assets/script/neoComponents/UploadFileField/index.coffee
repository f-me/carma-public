###
Implementation of upload-file field.

It is generic and configurable, used in multiple places.
You could see it on "case" screen for example.
###

{ko} = require "carma/vendor"


class UploadFileFieldViewModel
  constructor: ({
    @isMultiple    = no   # Allow user to select many files
    @withButton    = yes  # Show "upload" button to trigger `@uploadHandler`

    @autoTrigger   = no   # Automatically trigger `@uploadHandler` when file is
                          # selected.

    @autoClear     = yes  # Automatically empty `@selectedFiles` when
                          # `@uploadClickHandler` is triggered.

    @mimeFilter    = null # MIME-type string ("accept" input:file's attribute).
                          # Filters files in file-dialog by this MIME-type.

    @fileInputId   = null # When you need to link it with some <label>
    @uploadHandler = (->) # First arguments is [File] list (from input:file)

    @selectedFiles = ko.observableArray []
                          # Bidirectional binding so you could clear selected
                          # list outside.

    @isLoading     = ko.observable no
                          # Show a spinner and temporarly block handlers
  }) ->
    @viewSelectedFiles = ko.pureComputed =>
      files = @selectedFiles()

      switch files.length
        when 0 then ""
        when 1 then files[0].name
        else "(#{files.length}) #{(name for {name} in files).join "; "}"

  dispose: =>

  fileInputChangeHandler: (model, ev) =>
    return if @isLoading()
    @selectedFiles (x for x in ev.currentTarget.files)
    do @uploadClickHandler if @autoTrigger

  uploadClickHandler: =>
    return if @isLoading()
    files = (x for x in @selectedFiles())
    @selectedFiles [] if @autoClear
    @uploadHandler files


module.exports =
  componentName: "upload-file-field"

  component:
    template:  require "./template.pug"
    viewModel: UploadFileFieldViewModel
