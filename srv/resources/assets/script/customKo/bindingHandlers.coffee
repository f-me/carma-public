{$, _, ko, Mustache} = require "carma/vendor"

ko.bindingHandlers.setdata =
  init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
    # This will be called when the binding is first applied to an element
    # Set up any initial state, event handlers, etc. here
    $(element).data knockVM: viewModel, acc: valueAccessor

ko.bindingHandlers.bindClick =
  init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
    fn = valueAccessor()
    $(element).click ->
      fn(element, viewModel)

ko.bindingHandlers.updMultiDict =
  init: (element, valueAccessor, allBindingsAccessor, kvm) ->
    fn = valueAccessor()

ko.bindingHandlers.disabled =
  update: (el, acc, allBindigns, kvm) ->
    $(el).attr('disabled', acc()())

ko.bindingHandlers.readonly =
  update: (el, acc, allBindigns, kvm) ->
    $(el).attr('readonly', acc()())

ko.bindingHandlers.sync =
  update: (el, acc, allBindings) ->
    isSync = ko.utils.unwrapObservable acc()
    if isSync
      $(el).fadeIn 'fast'
    else
      $(el).fadeOut 'slow'

ko.bindingHandlers.spinner =
  update: (el, acc, allBindings) ->
    showSpinner = ko.utils.unwrapObservable acc()
    if showSpinner
      $(el).children(':not(.spinner)').each (index, element) ->
        $(element).addClass "blur"
      $(el).spin 'huge', '#777'
    else
      $(el).children(':not(.spinner)').each (index, element) ->
        $(element).removeClass "blur"
      $(el).spin false

ko.bindingHandlers.pickerDisable =
  update: (el, acc, allBindigns, kvm) ->
    $(el).data('disabled', acc()())

ko.bindingHandlers.bindDict =
  init: (el, acc, allBindigns, kvm) ->
    # FIXME: This is hack to support legacy kvm field
    if _.isString acc()
      th = kvm["#{acc()}TypeaheadBuilder"]()
    else
      th = acc().typeaheadBuilder()
    th.setElement(el)
    # Do not open TH menu if the field is non-writable
    if _.isString acc()
      fld = _.find kvm._meta.model.fields, (f) -> f.name == acc()
    else
      fld = _.find acc().kvm._meta.model.fields, (f) -> f.name == acc()
    # bind th.draw here, because we don't have ready th
    # during binding any more, see bug #1148
    chevron = $(el).siblings().find('.glyphicon-chevron-down').parent()[0]
    if chevron
      $(chevron).on 'click', th.drawAll unless fld?.readonly

    search = $(el).siblings().find('.glyphicon-search').parent()[0]
    if search
      $(search).on 'click', th.drawAllForce unless fld?.readonly

funToggle = (fns...) ->
  i = 0
  l = fns.length
  -> fns[i++ % l]()

ko.bindingHandlers.sort =
  init: (el, name, allBindings, viewModel, ctx) ->
    # add icon to show sorting direction
    defaultClass = 'icon-resize-vertical'
    $(el).prepend("<i class=#{defaultClass}></i>")
    # toggle sorting direction when user clicks on column header
    $(el).click funToggle(
      ->
        # reset icon for others columns
        resetSort el, defaultClass
        # change icon to sorting ascending
        $(el).find('i').removeClass()
        $(el).find('i').addClass 'icon-arrow-up'
        # launch sorting
        ctx.$root.kvms.set_sorter name(), "asc"
      ->
        # reset icon for others columns
        resetSort el, defaultClass
        # change icon to sorting descending
        $(el).find('i').removeClass()
        $(el).find('i').addClass 'icon-arrow-down'
        # launch sorting
        ctx.$root.kvms.set_sorter name(), "desc"
    )
    # reset icon to default (without sorting) for all column headers
    resetSort = (el, defaultClass) ->
      $(el).closest('thead')
           .children()
           .find('.icon-arrow-down, .icon-arrow-up')
           .removeClass()
           .addClass(defaultClass)

ko.bindingHandlers.renderField =
  init: (el, acc, allBindigns, fld, ctx) ->
    return if acc().meta.invisible
    tplid = acc().meta?.widget || acc().type || 'text'
    tplid = "dictionary-many" if /^dictionary-set/.test(acc().type)
    tplid = "text" if acc().type == "ident"
    tpl   = Mustache.render $("##{tplid}-field-template").html(), acc()

    if ctx.$root.wrapFields
      # use some magick: wrap with div, so parser can grab all content
      # and serialize and parse again to make copy of template, so we
      # can populate ".content" part of the copy
      wrap = $("<div/>").html($("##{ctx.$root.wrapFields}-template").html())
      wrap.find(".content").html($(tpl))
      # use special context for wrappers, so we will know what field we are
      # rendering
      context = { kvm: ctx.$root.kvm, fld: fld }

    tpl = wrap.html() if wrap

    # use default context for usual fields so we can use default templates
    context ?= ctx.$root.kvm
    ko.utils.setHtml el, tpl
    ko.applyBindingsToDescendants(context, el)
    controlsDescendantBindings: true

ko.bindingHandlers.renderGroup =
  init: (el, acc, allBindigns, fld, ctx) ->
    group = ctx.$root.showFields.groups[fld]
    fs = []
    for modelName, fields of group
      fs.push({kvm: acc()[modelName], field: fld}) for fld in fields

    ko.utils.setHtml el, $("#group-ro-template").html()
    ko.applyBindingsToDescendants({ fields: fs}, el)
    controlsDescendantBindings: true

ko.bindingHandlers.render =
  init: (el, acc, allBindigns, ctx, koctx) ->
    # console.log 'render', el, acc()
    return unless acc().field
    fld = acc().kvm[acc().field.name]
    ko.bindingHandlers.fieldRender.init(el, (-> fld), allBindigns, ctx, koctx)


ko.bindingHandlers.fieldRender =
  init: (el, acc, allBindigns, ctx, koctx) ->
    fld = acc()
    tplName = fld.field.meta?.widget || fld.field.type || 'text'
    tpl = $("##{tplName}-ro-template").html()
    console.error "Cant find template for #{tplName}" unless tpl
    ko.utils.setHtml el, tpl
    ko.applyBindingsToDescendants(acc().kvm[acc().field.name], el)
    controlsDescendantBindings: true

ko.bindingHandlers.expandAll =
  init: (el, acc, allBindigns, ctx, koctx) ->
    $(el).append("<label><span class='glyphicon glyphicon-plus'></span></label>")
    $(el).click ->
      expanded = $(el).find('span').hasClass('glyphicon-minus')
      $(el).closest('table').find('.expand-contoller').each (key, tr) ->
        if expanded is $(tr).hasClass('expanded')
          $(tr).trigger 'click'
      $(el).find('span').toggleClass('glyphicon-plus').toggleClass('glyphicon-minus')

ko.bindingHandlers.expand =
  init: (el, acc, allBindigns, ctx, koctx) ->
    $(el).append("<label><span class='glyphicon glyphicon-plus'></span></label>")
    $(el).click ->
      $(el).parent().next().toggleClass('hide')
      $(el).toggleClass('expanded')
      $(el).find('span').toggleClass('glyphicon-plus').toggleClass('glyphicon-minus')

ko.bindingHandlers.eachNonEmpty =
  nonEmpty: (fnames, ctx, koctx) ->
    _.reject fnames, (fname) ->
      g = koctx.$root.showFields.groups[fname]
      _.all (_.keys g), (m) ->
        kvm = ctx[m]
        _.all g[m], (f) ->
          v = kvm[f.name]()
          if f
            (_.isArray v and _.isEmpty v) or _.isNull v
          else
            true

  init: (el, acc, allBindigns, ctx, koctx) ->
    fnames = ko.utils.unwrapObservable acc()
    fns = ko.bindingHandlers.eachNonEmpty.nonEmpty fnames, ctx, koctx
    ko.applyBindingsToNode el, {foreach: fns}, koctx
    controlsDescendantBindings: true

ko.bindingHandlers.addMask =
  init: (el, acc) ->
    switch acc()
      when "datetime" then $(el).inputmask mask: "99.99.9999 99:99:99"
      else console.error("unknown mask")

# "typeahead" handler
# params model (every params here is optional):
#   options:  Object (see for docs of typeahead.js)
#   datasets: [Object (see for docs of typeahead.js)]
#   value:    Observable (knockout's) - to set selected value there
do ->
  defaultOptions = Object.freeze
    highlight: true
    minLength: 0

    classNames: Object.freeze
      open: "open tt-open"
      dataset: "dropdown-menu tt-dataset"

  defaultDatasets = Object.freeze
    source: []

    templates: Object.freeze
      suggestion: (value) -> "<li><a href='#'>#{value}</a></li>"

      notFound: () ->
        "<li class='disabled'><a href='#'>Ничего не найдено</a></li>"

  makeDataset = (x) ->
    x ?= {}
    templates = Object.assign {}, defaultDatasets.templates, (x.templates ? {})
    Object.assign {}, defaultDatasets, x, {templates}

  dispose = (el) ->
    el.__ko_typeahead_unsubscribe?()
    delete el.__ko_typeahead_unsubscribe

    $(el)
      .off "typeahead:select.__ko_typeahead"
      .typeahead "destroy"

  init = (el, acc) ->
    accVal = ko.utils.unwrapObservable acc()
    return unless accVal # disabled by falsy value
    o = if typeof accVal is "object" then accVal else {}

    classNames = Object.assign {},
      defaultOptions.classNames,
      (o.options?.classNames ? {})

    options = Object.assign {},
      defaultOptions,
      (o.options ? {}),
      {classNames}

    args = [options].concat (makeDataset x for x in o.datasets ? [])
    $el = $ el
    $el.typeahead.apply $el, args

    if o.value?
      lastValue = o.value()

      el.__ko_typeahead_unsubscribe = o.value.subscribe (x) ->
        $el.typeahead "val", x if x isnt lastValue

      $el.on "typeahead:select.__ko_typeahead", (e, x) ->
        lastValue = x
        o.value x

    ko.utils.domNodeDisposal.addDisposeCallback el,
      dispose.bind null, arguments...

  update = (args...) ->
    dispose args...
    init args...

  ko.bindingHandlers.typeahead = {init, update}

# "datepicker" handler
do ->
  defaultOptions = Object.freeze
    autoclose        : true
    enableOnReadonly : false
    format           : "dd.mm.yyyy"
    weekStart        : 1

  dispose = (el) ->
    $(el)
      .off "changeDate.__ko_datepicker"
      .datepicker "destroy"

  init = (el, acc, allBindingsAcc) ->
    accVal = ko.utils.unwrapObservable acc()
    return unless accVal # disabled by falsy value
    o = if typeof accVal is "object" then accVal else {}

    $(el)
      .datepicker Object.assign {}, defaultOptions, o
      .on "changeDate.__ko_datepicker", ->
        allBindings = allBindingsAcc()
        allBindings.value @value if ko.isObservable allBindings.value

    ko.utils.domNodeDisposal.addDisposeCallback el,
      dispose.bind null, arguments...

  update = (args...) ->
    dispose args...
    init args...

  ko.bindingHandlers.datepicker = {init, update}
