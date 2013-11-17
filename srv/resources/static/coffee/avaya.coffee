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
      panel.find(".search-query")
        .val(number)
        .css("background", if number in redNumbers then "coral" else "white")

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


redNumbers =
  [ "+79851996802"
  , "+79151747332"
  , "+79067510641"
  , "+79623619523"
  , "+79267474413"
  , "+79162169927"
  , "+79168454204"
  , "+79264304598"
  , "+79851996802"
  , "+79851996809"
  , "+79255034127"
  , "+79255034102"
  , "+79255034128"
  , "+79255034129"
  , "+79255037101"
  , "+79255037111"
  , "+79255037109"
  , "+79169607170"
  , "+79161449827"
  , "+79169362625"
  , "+79175821619"
  , "+79165174737"
  , "+79687682060"
  , "+79852031016"
  , "+79652243267"
  , "+79033092959"
  , "+79037232050"
  , "+79685318575"
  , "+79685318602"
  , "+79611733103"
  , "+79851996802"
  , "+79851996809"
  , "+79851996807"
  , "+79067898262"
  , "+79150220967"
  , "+79151100615"
  , "+79153899661"
  , "+79150210362"
  , "+79150611145"
  ]


# FIXME: make a real dictionary from this
lineInfo =
  "PEUGEOT+B":
    greeting: "Пежо ассистанс, имя оператора, здравствуйте."
    program: "peugeot"
  "CITROEN+B":
    greeting: "Ситроен ассистанс, имя оператора, здравствуйте."
    program: "citroen"
  "VW+B":
    greeting: "VW Гарантия мобильности, имя оператора, чем могу Вам помочь?"
    program: "vwMotor"
  "GMKOR+B":
    greeting: "GM ассистанс, добрый день, чем могу Вам помочь?"
    program: "chevyko"
  "GM+B":
    greeting: "GM ассистанс, добрый день, чем могу Вам помочь?"
    program: "opel"
  "FORD+B":
    greeting: "Ford помощь на дорогах, имя оператора, добрый день, чем могу Вам помочь?"
    program: "ford"
  "ARC CLUBS+B":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "arc"
  "RAMC B2C+B":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "b2cSt"
  "RUS-LAN+B":
    greeting: "Рус-Лан ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "ruslan"
  "ATLANT-M+B":
    greeting: "Атлант М Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "atlant"
  "AVILON+B":
    greeting: "Авилон ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "avilon"
  "NEZAVISIMOST+B":
    greeting: "Независимость Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "nz"
  "EUROPLAN+B":
    greeting: "Европлан Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "euro"
  "MAPFRE+B":
    greeting: "Ассистанс центр МАПФРЕ УОРРЭНТИ, добрый день, чем могу Вам помочь?"
    program: "map"
  "VW VNUKOVO+B":
    greeting: "Фольксваген Внуково Ассистанс, имя оператора, добрый день, чем могу Вам помочь?"
    program: "vnukovo"
  "UNICREDITBANK":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "unicredit"
  "VTB 24":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: "vtb24"
  "RAMC 1/2+B":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: ""
  "RAMC 1/2+B":
    greeting: "Русский АвтоМотоКлуб, имя оператора, добрый день! (Здравствуйте!)"
    program: ""
