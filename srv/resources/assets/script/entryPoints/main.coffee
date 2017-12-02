require "carma/vendor"
require "carma/customKo"
require "carma-css"

{request} = require "carma/data"

document.getElementById("top-navbar").innerHTML =
  require "carma-tpl/partials/navbar.pug"

document.getElementById("top-level-modals").innerHTML =
  require "carma-tpl/partials/topLevelModals.pug"

request()
  .then \

    ({cfg: {dicts}, model: {user, users}}) ->

      # DOM is already ready
      # (because scripts connected at bottom of the page).
      require("carma/local").init {dicts, user, users}

    , (err) ->
      window.alert "Произошла ошибка при загрузке данных"
      throw err

  .catch (err) ->
    window.alert "Произошла ошибка при инициализации CaRMa"
    setTimeout -> throw err
