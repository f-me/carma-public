ko.bindingHandlers.setdata =
  init: (element, valueAccessor, allBindingsAccessor, viewModel) ->
    # This will be called when the binding is first applied to an element
    # Set up any initial state, event handlers, etc. here
    $(element).data({ knockVM: viewModel, acc: valueAccessor})

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

ko.bindingHandlers.pickerDisable =
  update: (el, acc, allBindigns, kvm) ->
    $(el).data('disabled', acc()())

ko.bindingHandlers.bindDict =
  init: (el, acc, allBindigns, kvm) ->
    kvm["#{acc()}Typeahead"].setElement(el)

ko.bindingHandlers.render =
  init: (el, acc, allBindigns, ctx) ->
    tpl = acc()._meta.tpls[ctx.name]
    ko.utils.setHtml el, tpl

ko.bindingHandlers.sort =
  update: (el, name, allBindings, viewModel, ctx) ->
    $(el).prepend("<i class='icon-resize-vertical'></i>")
    $(el).toggle(
      () ->
        $(el).find('i').removeClass()
        $(el).find('i').addClass 'icon-arrow-up'
        ctx.$root["#{name()}SortASC"]()
      ,
      () ->
        $(el).find('i').removeClass()
        $(el).find('i').addClass 'icon-arrow-down'
        ctx.$root["#{name()}SortDSC"]()
    )

