{_} = require "carma/vendor"
{urlFor} = require "carma/globallibs/utils"

# helper for rendering templates
tpl = (tpl, data = {}) -> tpl _.extend({_, urlFor}, data)

module.exports = {tpl}
