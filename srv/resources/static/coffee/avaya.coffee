
class Phone
  constructor: (ext,pwd) ->
    url = "ws://#{location.hostname}:8001/avaya/#{ext}/#{pwd}"
    @ws = new WebSocket(url)
    @ws.onopen = ->
      @connected()

    @ws.onclose = ->
      @destructor()

    @ws.onerror = ->
      @destructor()

    @ws.onmessage = (ev) ->
      msg = JSON.parse(ev.data)
      if msg.type == "ringer"
        if msg.ringer == "ringing"
          @calling()
        else
          @notCalling()
      else if msg.type == "display"    
        m = msg.display.match(/a=\s*(\d+)\s*to\s*(.+)/)
        if m
          number = m[1].replace(/^(98|89|)/,'+7')
          @callInfo(number, m[2])

  acceptCall: ->
    @ws.send('acceptCall')



class @AvayaWidget
  constructor: (panel, ext, pwd) ->
    phone = new Phone(ext, pwd)
    @__phone = phone # test hook
    phone.connected = ->
      panel.show()
      panel.find('#avaya-accept').click ->
        phone.acceptCall()

    phone.destructor = ->
      panel.hide()

    phone.calling = ->
      panel.addClass("open")

    phone.notCalling = ->
      panel.removeClass("open")
  
    phone.callInfo = (number, line) ->
      panel.find("#avaya-info").text(lineInfo[line].greeting)
      panel.find(".search-query").val(number)
      
lineInfo =
  "VW":
    greeting: "VW Гарантия мобильности, имя оператора, чем могу Вам помочь?"
    program: "VW / Легковые автомобили"
  "GM KOREA":
    greeting: "GM ассистанс, добрый день, чем могу Вам помочь?"
    program: "GM / Chevrolet Korea"
  "GM":
    greeting: "GM ассистанс, добрый день, чем могу Вам помочь?"
    program: "GM / Cadillac до 2012"
  "FORD":
    greeting: "Ford помощь на дорогах, имя оператора, добрый день, чем могу Вам помочь?"
    program: "Ford"
  "ARC CLUBS":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "B2B / Arc B2B"
  "RAMC B2C":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "B2C"
  "RUS-LAN":
    greeting: "Рус-Лан ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "Рус Лан"
  "ATLANT-M":
    greeting: "Атлант М Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "Атлант М"
  "CHARTIS":
    greeting: "Надёжный патруль Чартис, имя оператора, добрый день, чем могу Вам помочь?"
    program: "Chartis Assistance"
  "VW AVILON":
    greeting: "Авилон ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "B2B / Авилон"
  "NEZAVISIMOST":
    greeting: "Независимость Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "Независимость"
  "EUROPLAN":
    greeting: "Европлан Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "B2B / Европлан"
  "MAPFRE":
    greeting: "Ассистанс центр МАПФРЕ УОРРЭНТИ, добрый день, чем могу Вам помочь?"
    program: "B2B / Мапфре"
  "FWC VNUKOVO":
    greeting: "Фольксваген Внуково Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "B2B / VW Внуково"
  "RN CART":
    greeting: "Москва Помощь на Дорогах, имя оператора, добрый день, чем могу Вам помочь?"
    program: "B2B / РН-карт-Москва Базовая"
  "UNICREDITBANK":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "B2C / ЮниКредитбанк"
  "VTB 24":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "B2C / ВТБ 24"
  "RAMC":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: ""
  "RAMC 2":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: ""
