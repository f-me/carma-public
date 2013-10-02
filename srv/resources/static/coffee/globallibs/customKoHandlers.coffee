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
    th = kvm["#{acc()}TypeaheadBuilder"]()
    th.setElement(el)
    # bind th.draw here, because we don't have ready th
    # during binding any more, see bug #1148
    $(el).next().on 'click', th.drawAll


ko.bindingHandlers.render =
  init: (el, acc, allBindigns, ctx) ->
    tpl = acc()._meta.tpls[ctx.name]
    ko.utils.setHtml el, tpl

ko.bindingHandlers.sort =
  update: (el, name, allBindings, viewModel, ctx) ->
    # add icon to show sorting direction
    defaultClass = 'icon-resize-vertical'
    $(el).prepend("<i class=#{defaultClass}></i>")

    # toggle sorting direction when user clicks on column header
    $(el).toggle(
      () ->
        # reset icon for others columns
        resetSort el, defaultClass
        # change icon to sorting ascending
        $(el).find('i').removeClass()
        $(el).find('i').addClass 'icon-arrow-up'
        # launch sorting
        ctx.$root.kvms.set_sorter "#{name()}SortAsc"
      ,
      () ->
        # reset icon for others columns
        resetSort el, defaultClass
        # change icon to sorting descending
        $(el).find('i').removeClass()
        $(el).find('i').addClass 'icon-arrow-down'
        # launch sorting
        ctx.$root.kvms.set_sorter "#{name()}SortDesc"
    )

    # reset icon to default (without sorting) for all column headers
    resetSort = (el, defaultClass) ->
      $(el).closest('thead')
           .children()
           .find('i')
           .removeClass()
           .addClass(defaultClass)

ko.bindingHandlers.renderField =
  init: (el, acc, allBindigns, fld, ctx) ->
    tplid = fld.meta.widget
    tplid = "#{fld.type || 'text'}"
    tpl = Mustache.render $("##{tplid}-field-template").html(), fld
    ko.utils.setHtml el, tpl
    ko.applyBindingsToDescendants(ctx.$root, el)
    return { controlsDescendantBindings: true }
