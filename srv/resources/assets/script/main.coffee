require "carma/vendor"
require "carma/globallibs"
require "carma-styles"

{request} = require "carma/data"

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
