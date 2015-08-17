
//var bower = __dirname + '/bower_components/';

var $ = require('jquery')
require('jquery_browser')
require('../../bower_components/normalize-css/normalize.css')

$(function() {
  $(".field").first().focus();
  var msg = "<div class='span6 offset2'>                             \
  Для использования системы необходим браузер Google chrome          \
  версии не ниже 10, установить его можно по следующей ссылке:       \
  <a href='http://www.google.com/chrome'> www.google.com/chrome </a> \
  </div>";
  if (!$.browser.mozilla && !$.browser.webkit) {
    $("body").html(msg);
  }
})
