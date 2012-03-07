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
    }
};

// Setup routing
localRouter = Backbone.Router.extend({
    // Must _not_ end with trailing slashes
    routes: {
        "case/:id": "loadCase",
        "case": "newCase",
        "search": "search"
    },

    loadCase: function (id) {
        renderScreen("case", {"id": id});
    },

    newCase: function () {
        renderScreen("case", {"id": null});
    },

    search: function () {
        renderScreen("search");
    }
});

// Case view
function setupCaseMain(viewName, args) {
    modelSetup
    ("case", {"service": "case-service-references"})
    (viewName, args.id, [], "case-permissions");
}

