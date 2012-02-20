function initBottomMenu() {
    //FIXME: navigating browser history with back/forward buttons does not
    //trigger routes.
    var MenuRouter = Backbone.Router.extend({
      routes: {
        "/:section": "setMenu",
        "/:section/*p": "setMenu"},

      setMenu: function(sect) {
        if (sect.length == 0) {
          this.navigate("/call", {trigger:true});
        } else {
          $("#menu a.menuitem.selected").removeClass("selected");
          $("#menu a.menuitem[href='/"+sect+"']").addClass("selected");
        }
      }
    });

    var menuRouter = new MenuRouter;

    $(".menuitem").click(function(e) {
      e.preventDefault();
      menuRouter.navigate(this.pathname, {trigger:true});
    });

    KeyboardJS.bind.key("ctrl+left", null, function(){
      $(".menuitem.selected").prev(".menuitem").click();
    });
    KeyboardJS.bind.key("ctrl+right", null, function(){
      $(".menuitem.selected").next(".menuitem").click();
    });

    return menuRouter;

}
