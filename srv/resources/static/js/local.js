/// Everything local to the customer resides here

localScreens = {
    "case":
    {
        "template": "case-screen-template",
        "views":
        {
            "case-form": setupCaseMain
        }
    },
    "search":
    {
        "template": "search-screen-template",
        "views":
        {
            "tableView": setupSearchTable
        }
    },
    "call":
    {
        "template": "call-screen-template",
        "views":
        {
            "call-form": setupCallForm
        }
    }
};

// Setup routing
localRouter = Backbone.Router.extend({
    // Must _not_ end with trailing slashes
    routes: {
        "case/:id": "loadCase",
        "case": "newCase",
        "search": "search",
        "call": "call"
    },

    loadCase: function (id) {
        renderScreen("case", {"id": id});
    },

    newCase: function () {
        renderScreen("case", {"id": null});
    },

    search: function () {
        renderScreen("search");
    },

    call: function () {
        renderScreen("call");
    }
});

$(function () {
    $.getJSON("/s/js/data/dictionaries.json",
          function(dicts) {
              mainSetup(localScreens, localRouter, dicts);
          });
});

// Case view
function setupCaseMain(viewName, args) {
    
    var rf = "services";
    // Where to render services views
    var forest = "case-service-references";
    
    // Do the same for reference fields after first fetch() is
    // complete, but not if this case is new.
    // 
    // We must render reference views after the model has loaded
    // because the numer of refs is unknown when the model has not yet
    // been populated with data.
    //
    // Forms for referenced instances are then rendered with
    // modelSetup which means that viewsWare will be used for further
    // proper cleanup.
    //
    // We have to GO DEEPER, but recursion is not supported.
    var fetchCb = function(instance) {
        // Just once
        instance.unbind("change", fetchCb);
        if (!instance.isNew()) {
            books = setupMultiRef(instance, rf, forest);
            for (rn in books) {
                var subview = rf + "-view-" + rn;
                var setup = modelSetup(books[rn].refModelName);
                setup(subview, books[rn].refId,
                      { slotsee: [subview + "-link"],
                        permEl: subview + "-perms"});
            }
        }
    }

    modelSetup("case")(viewName, args.id, 
                       {fetchCb: fetchCb, 
                        permEl: "case-permissions"});

}

// Top-level wrapper for storeService, which grabs serviceModelName
// from service-picker
function addService() {
    addReference(global.viewsWare["case-form"].bbInstance,
                 "services",
                 $("[name=service-picker]").val(),
                 "case-service-references");
}

function setupCallForm (viewName, args) {
    modelSetup("call")(viewName, null,
                       {permEl: "case-permissions"});
}


function initOSM() {
      window.osmap = new OpenLayers.Map("basicMap");
      var mapnik = new OpenLayers.Layer.OSM();
      osmap.addLayer(mapnik);
      osmap.setCenter(new OpenLayers.LonLat(37.617874,55.757549) // Center of the map
        .transform(
          new OpenLayers.Projection("EPSG:4326"), // transform from WGS 1984
          new OpenLayers.Projection("EPSG:900913") // to Spherical Mercator Projection
        ), 16 // Zoom level
      );
}
