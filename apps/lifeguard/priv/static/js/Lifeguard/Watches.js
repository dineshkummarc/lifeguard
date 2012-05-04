var Watch = Backbone.Model.extend({
    idAttribute: "name",
    urlRoot: "/api/watches",

    defaults: {
        "name": "<unknown>",
        "code": "",
        "interval": ""
    }
});

var Watches = Backbone.Collection.extend({
    model: Watch,
    url: "/api/watches",

    parse: function(response) {
        return response.watches;
    }
});

var WatchView = Backbone.View.extend({
    tagName: "tr",
    template: null,

    events: {
        "click .edit": "edit",
        "click .delete": "delete"
    },

    initialize: function() {
        if (!WatchView.template) {
            var templateEl = document.id("watch-template");
            var template   = _.template(templateEl.get("html"));
            WatchView.prototype.template = template;
        }

        this.model.on("change", this.render, this);
        this.model.on("destroy", this.remove, this);
    },

    edit: function(event) {
        event.preventDefault();
        this.trigger("editWatch", this.model);
    },

    delete: function(event) {
        event.preventDefault();

        // Delete it!
        if (window.confirm("Are you sure? Deletes are final.")) {
            dbg.info("Deleting model: " + this.model.get("name"));
            this.model.destroy();
        }
    },

    render: function() {
        this.$el.html(this.template(this.model.toJSON()));
        return this;
    }
});

var WatchListView = Backbone.View.extend({
    el: document.id("watches-list"),

    events: {
        "click .new":  "newWatch"
    },

    initialize: function() {
        this.table      = this.$("table.watches tbody.watches")[0];
        this.no_watches = this.$(".no-watches")[0];

        this.collection.on("add", this.addOne, this);
        this.collection.on("reset", this.addAll, this);
        this.collection.on("all", this.render, this);

        // Kick off the fetch to grab all our watches
        this.collection.fetch();
    },

    render: function() {
        if (this.collection.length) {
            this.no_watches.setStyle("display", "none");
        } else {
            this.no_watches.setStyle("display", "table-row-group");
        }
    },

    addAll: function() {
        dbg.debug("Adding all elements from the watches collection");
        this.collection.each(this.addOne, this);
    },

    addOne: function(watch) {
        // Create the view and attach any relevant event listeners
        var view = new WatchView({ model: watch });
        view.on("editWatch", function(watch) {
            this.trigger("editWatch", watch);
        }, this);

        // Render the view and put it in our table
        var el   = view.render().el;
        el.inject(this.table);
    },

    hide: function() {
        this.el.setStyle("display", "none");
    },

    newWatch: function(event) {
        event.preventDefault();
        this.trigger("newWatch");
    },

    show: function() {
        this.el.setStyle("display", "block");
    }
});

var WatchFormView = Backbone.View.extend({
    tagName: "div",
    template: null,

    events: {
        "click .cancel": "cancel",
        "click .save": "save"
    },

    initialize: function() {
        if (!WatchFormView.template) {
            var templateEl = document.id("watch-new-template");
            var template   = _.template(templateEl.get("html"));;
            WatchFormView.prototype.template = template;

            var errorTemplateEl = document.id("watch-new-errors-template");
            var errorTemplate = _.template(errorTemplateEl.get("html"));
            WatchFormView.prototype.errorTemplate = errorTemplate;
        }

        // If we're given a model, then we're modifying an existing
        // Watch. Otherwise, we're creating a new one.
        if (this.model)
            this.isNew = false;
        else
            this.isNew = true;

        this.rendered = false;
        this.editor = null;
    },

    render: function() {
        if (!this.rendered) {
            // Render and mark as rendered
            this.$el.html(this.template());
            this.rendered = true;

            // Get some of the common elements
            this.save_button = this.el.getElement(".save");
       }

        if (this.editor === null) {
            // Setup the code editor
            var editor  = window.ace.edit("editor");
            var session = editor.getSession();

            // Save the editor to our instance
            this.editor = editor;

            // Default settings to make code editing a better experience
            editor.renderer.setShowGutter(true);
            editor.renderer.setHScrollBarAlwaysVisible(false);
            editor.setHighlightSelectedWord(true);
            editor.setShowInvisibles(false);
            editor.setShowPrintMargin(false);
            session.setUseSoftTabs(false);
            session.setUseWrapMode(true);

            // We always modify JS so set the mode to JS
            var JavaScriptMode = require("ace/mode/javascript").Mode;
            session.setMode(new JavaScriptMode());
        }

        if (!this.isNew) {
            // Since we're modifying an existing model, update all
            // the form values with the model data
            this.el.getElementById("name").value = this.model.get("name");
            this.el.getElementById("interval").value = this.model.get("interval");
            this.editor.getSession().setValue(this.model.get("code"));
        }

        return this;
    },

    cancel: function(event) {
        event.preventDefault();

        if (window.confirm("Are you sure? Your changes will be lost.")) {
            this.trigger("cancel");
        }
    },

    save: function(event) {
        event.preventDefault();

        // Disable the save button
        this.save_button.addClass("disabled");

        var watch = this.model;
        if (this.isNew)
            watch = new Watch();

        // Set the attributes
        watch.set({
            name: this.el.getElementById("name").value,
            interval: parseInt(this.el.getElementById("interval").value, 10),
            code: this.editor.getSession().getValue()
        });

        // Save it
        watch.save(null, {
            success: _.bind(this.saveSuccess, this),
            error: _.bind(this.saveError, this)
        });
    },

    saveError: function(model, response) {
        // Parse the errors and show the message
        var json      = JSON.parse(response.responseText);
        var errorHtml = this.errorTemplate({ errors: json.errors });
        this.el.getElementById("errors").set("html", errorHtml);

        // Re-enable the save button
        this.save_button.removeClass("disabled");
    },

    saveSuccess: function(model, response) {
        if (this.isNew) {
            // Add the model to our collection
            this.collection.add(model);
        }

        // Trigger an event to get us out of here
        this.trigger("saveComplete");
    }
});

var WatchRouter = Backbone.Router.extend({
    routes: {
        "watches":      "listWatches",
        "watches/new":  "newWatch",
        "watches/edit/:id": "editWatch"
    },

    initialize: function() {
        // Get the main container that stores the active view
        this.container = document.id("view-container");

        // This is the collection of Watches used by the main list view
        this.collection = new Watches();

        // This is a collection of watches that are used only internally
        this.internal_collection = new Watches();

        // This is the main list view, which is always around but sometimes
        // hidden. This gets created initially in listWatches.
        this.list_view = null;

        // This is the current view showing
        this.current_view = null;
    },

    editWatch: function(id) {
        var watch = this.collection.get(id);
        if (!watch) {
            // Attempt to load it from the internal collection...
            watch = this.internal_collection.get(id);
        }

        // If we don't have it in the collection, attempt to fetch from
        // the server.
        if (!watch) {
            dbg.info("Model not found, attempting to fetch from server...");
            watch = new Watch({ name: id });
            watch.fetch({
                success: _.bind(function(model, response) {
                    this.internal_collection.add(model);
                    Backbone.history.loadUrl();
                }, this)
            });
            return;
        }

        // Otherwise show the form
        this._watchForm({ model: watch });
    },

    listWatches: function() {
        if (this.list_view === null) {
            // Create the view pointing to our collection
            this.list_view = new WatchListView({ collection: this.collection });

            // Bind to the events on the view
            this.list_view.on("newWatch", function() {
                this.navigate("watches/new", { trigger: true });
            }, this);

            this.list_view.on("editWatch", function(watch) {
                // We reset the internal collection here as some bookkeeping
                // to avoid just growing this unbounded
                this.internal_collection.reset();

                // Navigate to the edit page
                this.navigate("watches/edit/" + watch.id, { trigger: true });
            }, this);
        }

        // Show it!
        this._setView(this.list_view);
    },

    newWatch: function() {
        this._watchForm({ collection: this.collection });
    },

    _setView: function(view) {
        // If the view we're setting to is equal to our list view, then
        // we need to remove the current view
        if (this.current_view !== null)
            this.current_view.remove();

        if (this.list_view !== null) {
            if (this.list_view === view) {
                // Show the list view again and return since we're done
                this.list_view.show();
                return;
            } else {
                // We're showing some other view, so just hide the main
                // list view.
                this.list_view.hide();
            }
        }

        this.container.grab(view.el);
        this.current_view = view;
    },

    _watchForm: function(options) {
        // Create the new view
        var view = new WatchFormView(options);
        view.on("cancel", function() {
            this.navigate("watches", { trigger: true });
        }, this);

        view.on("saveComplete", function() {
            this.navigate("watches", { trigger: true });
        }, this);

        // Swap in our new view
        this._setView(view);

        // Render. Note that this MUST be called after _setView because
        // the ACE editor requires the elements to be visible.
        view.render();
    }
});
