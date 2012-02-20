function initBottomMenu() {
    var MenuRouter = Backbone.Router.extend({
      routes: {
        ":section": "updMenu",
        ":section/*p": "updMenu"
      },

      updMenu: function(sect) {
        $("#menu a.menuitem.selected").removeClass("selected");
        $("#menu a.menuitem[href='"+sect+"']").addClass("selected");
      }
    });

    var menuRouter = new MenuRouter;

    $(".menuitem").click(function(e) {
      e.preventDefault();
      //`pathname` contains leading slash, we need to remove it.
      //otherwise route handler (updMenu) will receive empty argument
      var path = this.pathname.substring(1);
      menuRouter.navigate(path, {trigger:true});
    });

    KeyboardJS.bind.key("ctrl+left", null, function(){
      $(".menuitem.selected").prev(".menuitem").click();
    });
    KeyboardJS.bind.key("ctrl+right", null, function(){
      $(".menuitem.selected").next(".menuitem").click();
    });

    return menuRouter;

}
