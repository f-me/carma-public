
function createCatComplete(elem, data, params) {

  return elem.catcomplete(
    _.extend(
      {
        delay: 0,
        source: data,
        minLength: 0,
        autoFocus: true
      },
      params))
    .focus(function() {
      $(this).trigger("keydown.autocomplete");
    });
}

function transformForCatComplete(items) {  
  if (_.isUndefined(items)) { return []; }
  if (items.length === 0 || !_.has(items[0],"cat")) {
    items = [{cat:"", sub:items}];
  }
  var data = [];
  _.each(items, function(cat) {
    _.each(cat.sub, function(sub) {
      var i = data.length;
      data[i] = {category: cat.cat, text: sub, id:i, label: (i+1) + "  " + sub};
    });
  });
  return data;
}

$.widget("custom.catcomplete", $.ui.autocomplete, {
  _renderMenu: function(ul, items) {
      var self = this;
      var currentCategory = "";
      $.each(items, function(index, item) {
        if (item.category != currentCategory) {
          ul.append("<li class='ui-autocomplete-category'>" + item.category + "</li>");
          currentCategory = item.category;
        }
        self._renderItem(ul, item);
    });
  }
});
