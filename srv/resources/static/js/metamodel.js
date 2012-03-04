/// Backbonize a model, set default values for model
///
/// @return Constructor of Backbone model
function backbonizeModel(model, modelName) {
    var defaults = {};
    var fieldHash = {};
    _.each(model.fields,
          function(f) {
              if (!(_.isUndefined(f.default)))
                  defaults[f.name] = f.default;
              else
                  defaults[f.name] = null;
              fieldHash[f.name] = f;
          });

    var M = Backbone.Model.extend({
        defaults: defaults,
        /// Temporary storage for attributes queued for sending to
        /// server
        attributeQueue: {},
        initialize: function() {
            if (!this.isNew())
                this.fetch();
        },
        model: model,
        fieldHash: fieldHash,
        /// Bind model changes to server sync
        setupServerSync: function () {
            var realUpdates = function () {
                /// Do not resave model when id is set after
                /// first POST
                if (!this.hasChanged("id"))
                    this.save();
            };

            this.bind("change", _.throttle(realUpdates, 500), this);
        },
        set: function(attrs, options){
            Backbone.Model.prototype.set.call(this, attrs, options);
            /// Push new values in attributeQueue
            ///
            /// Never send "id", never send anything if user has no
            /// canUpdate permission.
            ///
            /// TODO _.extend doesn't work here
            for (k in attrs)
                if (k != "id" &&
                    this.model.canUpdate &&
                    this.fieldHash[k].canWrite &&
                    (!_.isNull(attrs[k])))
                    this.attributeQueue[k] = attrs[k];
        },
        /// Do not send empty updates to server
        save: function(attrs, options) {
            if (!_.isEmpty(this.attributeQueue))
                Backbone.Model.prototype.save.call(this, attrs, options);
        },
        /// For checkbox fields, translate "0"/"1" to false/true
        /// boolean.
        parse: function(json) {
            var m = this.model;
            for (k in json) {
                if ((k != "id") && (this.fieldHash[k].type == "checkbox")) {
                    if (json[k] == "1")
                        json[k] = true;
                    else
                        json[k] = false;
                }
            }
            return json;
        },        
        toJSON: function () {
            /// Send only attributeQueue instead of the whole object
            var json = this.attributeQueue;
            /// Map boolean values to string "0"/"1"'s for server
            /// compatibility
            for (k in json)
                if (_.isBoolean(json[k]))
                    json[k] = String(json[k] ? "1" : "0");
            this.attributeQueue = {};
            return json;
        },
        urlRoot: "/_/" + modelName
    });

    return M;
}

/// Convert model to forest of HTML form elements with appropriate
/// data-bind parameters for Knockout.
///
/// TODO: We can do this on server as well.
///
/// @return String with form HTML
function renderFormView(model) {
    var templates = [];

    _.each($(".field-template"),
           function(tmp) {
               templates[/\w+/.exec(tmp.id)[0]] = tmp.text;
           });

    var contents = "";
    var fType = "";
    var readonly = false;
    /// Pick an appropriate form widget for each model
    /// field type and render actual model value in it
    ///
    /// Set readonly context attribute for fields which have
    /// canEdit=false in form description.
    _.each(model.fields,
           function (f) {
             if (!f.invisible) {
               if (_.isUndefined(templates[f.type]))
                   fType = "unknown";
               else
                   fType = f.type;
               readonly = !model.canUpdate || !f.canWrite;
               contents += Mustache.render(templates[fType],
                                           _.extend(f, {readonly: readonly}));
             }
           });
    
    var modelRo = !model.canUpdate && !model.canCreate && !model.canDelete;
    /// Add HTML to contents for non-false permissions
    contents += Mustache.render($("#permission-template").text(), 
                                _.extend(model, {readonly: modelRo}));

    return contents;
}
