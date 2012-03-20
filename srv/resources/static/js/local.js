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
    var refs = [
        {
            field: "services",
            forest: "case-service-references",
        }
    ];
    _.extend(args, {callTaker: $("#realName").text(),
                    callDate: getFormatDate(),
                    callTime: getFormatTime()});
    modelSetup("case")(viewName, args, 
                       {permEl: "case-permissions",
                        slotsee: ["case-number"],
                        groupsForest: "right",
                        refs:refs});

    // Render service picker
    //
    // We use Bootstrap's glyphs if "icon" key is set in dictionary
    // entry.
    $("#service-picker-container").html(
        Mustache.render($("#service-picker-template").html(),
                        {dictionary: global.dictionaries["Services"]}));
}

// Return MM-DD-YYYY
function getFormatDate() {
    var d = new Date;
    var sd = (d.getMonth() + 1) + '-' + d.getDate() + '-' + d.getFullYear();
    return sd;
}

// Return HH:MM
function getFormatTime() {
    var d = new Date;
    var sd = d.getHours() + ':' + d.getMinutes();
    return sd;
}


// Scroll to the bottom of the page
function scrollDown() {
    window.scrollTo(0, document.body.scrollHeight);
}

// Hide all views on right pane and show view for first reference
// stored in <fieldName> of model loaded into <parentView> there
function showComplex(parentView, fieldName) {
    var refViewName = global.viewsWare[parentView].refViews[fieldName][0];
    $(".complex-field").hide();

    if (fieldName == "address") {
      $el(refViewName).show(
        function () { initOSM("coords"); });
    } else {
      $el(refViewName).show();
    }
}

// Return name field of refInstance the insight observable of parent
// instance must evaluate to.
function insightField(refInstance) {
    switch (refInstance.name) {
        case "caller": {
            return "callerName";
            break;
        }
        case "car": {
            return "vin";
            break;
        }
    }
    return "_none_";
}

// Top-level wrapper for storeService
function addService(name) {
    addReference(global.viewsWare["case-form"].bbInstance,
                 {field: "services",
                  modelName: name,
                  forest: "case-service-references"},
                "right");
}

function setupCallForm(viewName, args) {
    modelSetup("call")(viewName, null,
                       {permEl: "case-permissions"});
}

function initOSM(id) {
      window.global.osmap = new OpenLayers.Map(id);
      window.global.osmap.addLayer(new OpenLayers.Layer.OSM());
      window.global.osmap.setCenter(
        new OpenLayers.LonLat(37.617874,55.757549)
          .transform( // from WGS 1984 to Spherical Mercator Projection
            new OpenLayers.Projection("EPSG:4326"),
            new OpenLayers.Projection("EPSG:900913")
          ),
        16 // Zoom level
      );
}
