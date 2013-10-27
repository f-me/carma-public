class Phone
  constructor: (ext,pwd) ->
    url = "ws://#{location.hostname}:8001/avaya/#{ext}/#{pwd}"
    @ws = new WebSocket(url)
    @ws.onopen = => @connected()
    @ws.onclose = => @destructor()
    @ws.onerror = => @destructor()

    @ws.onmessage = (ev) =>
      msg = JSON.parse(ev.data)
      if msg.type == "ringer"
        if msg.ringer == "ringing"
          @calling()
        else
          @notCalling()
      else if msg.type == "display"
        m = msg.display.match(/a=\s*(\d+)\s*to\s*(.*\S)\s*/)
        if m
          number = m[1].replace(/^(98|8|)(\d{10})$/, '\+7$2')
          @callInfo(number, m[2])

  acceptCall: ->
    @ws.send('acceptCall')

  call: (number) ->
    @ws.send("dial:" + number.replace(/^\+7/,'98'))


class @AvayaWidget
  constructor: (panel, ext, pwd) ->
    phone = new Phone(ext, pwd)
    @__phone = phone # test hook
    phone.connected = ->
      panel.show()
      panel.find('#avaya-accept').click (e)->
        e.preventDefault()
        phone.acceptCall()
      panel.find('#avaya-call').click ->
        number = panel.find(".search-query").val()
        phone.call(number)

    phone.destructor = ->
      panel.hide()

    phone.calling = ->
      panel.addClass("open")

    phone.notCalling = ->
      panel.removeClass("open")

    phone.callInfo = (number, line) ->
      phone.calling()
      panel.find(".search-query").val(number)

      $("#search-query").val("!Тел:" + number)
      $("#search-query").change()

      vm = global.viewsWare['call-form'].knockVM
      vm.callerName_phone1(number)
      info = lineInfo[line]
      if info
        panel.find("#avaya-info").text(info.greeting)
        vm.program(info.program)

  call: (number) ->
    @__phone.call(number)


# FIXME: make a real dictionary from this
lineInfo =
  "PEUGEOT+Bosch":
    greeting: "Пежо ассистанс, имя оператора, здравствуйте."
    program: "peugeot"
  "CITROEN+Bosch":
    greeting: "Ситроен ассистанс, имя оператора, здравствуйте."
    program: "citroen"
  "VW+BOSCH":
    greeting: "VW Гарантия мобильности, имя оператора, чем могу Вам помочь?"
    program: "vwMotor"
  "GM KOREA":
    greeting: "GM ассистанс, добрый день, чем могу Вам помочь?"
    program: "chevyko"
  "GM+BOSCH":
    greeting: "GM ассистанс, добрый день, чем могу Вам помочь?"
    program: "opel"
  "FORD+BOSCH":
    greeting: "Ford помощь на дорогах, имя оператора, добрый день, чем могу Вам помочь?"
    program: "ford"
  "ARC CLUBS":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "arc"
  "RAMC B2C":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "b2cSt"
  "RUS-LAN":
    greeting: "Рус-Лан ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "ruslan"
  "ATLANT-M":
    greeting: "Атлант М Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "atlant"
  "VW AVILON":
    greeting: "Авилон ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "avilon"
  "NEZAVISIMOST":
    greeting: "Независимость Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "nz"
  "EUROPLAN":
    greeting: "Европлан Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "euro"
  "MAPFRE":
    greeting: "Ассистанс центр МАПФРЕ УОРРЭНТИ, добрый день, чем могу Вам помочь?"
    program: "map"
  "FWC VNUKOVO":
    greeting: "Фольксваген Внуково Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "vnukovo"
  "UNICREDITBANK":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "unicredit"
  "VTB 24":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "vtb24"
  "RAMC":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: ""
  "RAMC 2":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: ""
