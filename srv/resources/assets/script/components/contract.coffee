{$, _, ko, moment, Mustache} = require "carma/vendor"

Main    = require "carma/model/main"
Utils   = require "carma/utils"
DataMap = require "carma/sync/datamap"
Dict    = require "carma/dictionaries"
Flds    = require "carma-tpl/fields/form.pug"

flds = $('<div/>').append($(Flds))

ko.bindingHandlers.renderContract =
  update: (el, acc, allBindigns, contract, ctx) ->
    title = ko.utils.unwrapObservable acc()
    expired = ""
    # FIXME: move markup in templates
    unless contract.isExpired() is undefined
      expired = if contract.isExpired()
          "<span class='label label-danger'>Проверить участие</span>"
        else
          "<span class='label label-success'>Проверить условия</span>"
    close = "<button id='remove-contract'
                     title='Стереть из кейса ссылку на контракт'>
               <span class='glyphicon glyphicon-trash'/></button>"
    $(el).append("<h4>#{title} ##{contract.id()}
                   <small>#{expired} #{close}</small>
                 </h4>")
    $(el).find('#remove-contract').on 'click', -> contract.close()
    $dl = $("<table class='table table-condensed table-striped'></table>")
    $(el).append $dl
    _.each contract._meta.model.fields, (f) ->
      if f.type.indexOf("dictionary") != -1
        value = contract["#{f.name}Local"]()
      else
        value = contract[f.name]()

      if value and f.meta.label
        value = "<td>#{value}</td>"
        label = "<td><strong>#{f.meta.label}</strong></td>"
        $dl.append("<tr>#{label}#{value}</tr>")

    $dl.append(
      '<tr><td><strong>Предыдущие кейсы</strong></td>
           <td>
             <button id="contract-relevant-cases-btn">Скрыть</button>
             <ul id="contract-relevant-cases-lst">
               <span class="glyphicon glyphicon-refresh glyphicon-refresh-animate"></span>
             </ul>
           </td>
      </tr>')
    lst = $dl.find '#contract-relevant-cases-lst'
    btn = $dl.find '#contract-relevant-cases-btn'
    btn.on 'click', ->
      lst.toggle()
      btn.text(if lst.is(':visible') then 'Скрыть' else 'Показать')
    $.getJSON "/relevantCases/#{contract.caseId}", (res) ->
      relevantCases = for r in res
        "<li><a target='_blank' href='#case/#{r.caseId}'>#{r.caseDate} / #{r.caseId}</a></li>"
      if relevantCases.length
        lst.html(relevantCases.join '')
      else
        lst.html 'Ничего не найдено'

showContract = (contract, knockVM, el) ->
  contract.isActive = if contract.isActive then "Да" else "Нет"
  delete contract.dixi

  model = window.global.model 'Contract'
  mapper = new DataMap.Mapper(model)
  kvm = Main.buildKVM model, {fetched: mapper.s2cObj contract}

  kvm.caseId = knockVM.id()
  kvm.isExpired = ko.computed ->
    return unless knockVM.callDate?()
    callDate = moment(
        knockVM.callDate(),
        "DD.MM.YYYY HH:mm:ss"
      )?.startOf('day').format()
    validSince = moment(contract.validSince, "YYYY-MM-DD")?.format()
    validUntil = moment(contract.validUntil, "YYYY-MM-DD")?.format()
    callDate < validSince or callDate > validUntil

  kvm.close = ->
    return unless confirm "Стереть из кейса ссылку на контракт?"
    knockVM.contract("")

  $("##{el}").html(
    Mustache.render($(flds).find("#contract-content-template").html(),
                   {title: "Контракт"}))
  ko.applyBindings(kvm, $("#contract-content")[0])

hideContract = (el) ->
  $("##{el}").empty()

fetchContract = (id) ->
  JSON.parse ($.bgetJSON "/_/Contract/#{id}").responseText


module.exports =
  setup: (el, kvm) ->
    # show linked contract if it there
    if kvm["contract"]?()
      showContract (fetchContract kvm["contract"]()), kvm, el

    # change contract view, when user choose another contract
    kvm["contract"]?.subscribe (id) ->
      if id
        showContract (fetchContract id), kvm, el
      else
        hideContract el
