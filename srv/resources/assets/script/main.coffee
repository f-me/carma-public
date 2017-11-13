require "carma/vendor"
require "carma/globallibs"
require "carma-styles"

{request} = require "carma/data"
{tpl} = require "carma/globallibs/utils"

document.getElementById("top-navbar").innerHTML =
  tpl require "carma-tpl/partials/navbar.pug"

document.getElementById("top-level-modals").innerHTML =
  tpl require "carma-tpl/partials/topLevelModals.pug"

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
