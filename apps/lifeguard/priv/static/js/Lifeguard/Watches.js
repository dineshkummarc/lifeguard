var Watch = Backbone.Model.extend({
    idAttribute: "name",

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
    tagName: "tbody",
    template: null,

    events: {
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

    delete: function(event) {
        // Use MooTools to stop the default
        event.preventDefault();

        // Delete it!
        dbg.info("Deleting model: " + this.model.get("name"));
        this.model.destroy();
    },

    render: function() {
        this.$el.html(this.template(this.model.toJSON()));
        return this;
    }
});

var WatchListView = Backbone.View.extend({
    el: document.id("watches-list"),

    events: {
        "click .new": "newWatch"
    },

    initialize: function() {
        this.table      = this.$("table.watches")[0];
        this.no_watches = this.$(".no-watches")[0];

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
        var view = new WatchView({ model: watch });
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

var WatchNewView = Backbone.View.extend({
    tagName: "div",
    template: null,

    events: {
        "click .cancel": "cancel"
    },

    initialize: function() {
        if (!WatchNewView.template) {
            var templateEl = document.id("watch-new-template");
            var template   = _.template(templateEl.get("html"));
            WatchNewView.prototype.template = template;
        }

        this.rendered = false;
        this.editor = null;
    },

    render: function() {
        if (!this.rendered) {
            this.$el.html(this.template());

            // Mark that we rendered so that we don't do it again
            this.rendered = true;

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

        return this;
    },

    cancel: function(event) {
        event.preventDefault();
        this.trigger("cancel");
    }
});

var WatchRouter = Backbone.Router.extend({
    routes: {
        "watches":      "listWatches",
        "watches/new":  "newWatch"
    },

    initialize: function() {
        // Get the main container that stores the active view
        this.container = document.id("view-container");

        // This contains a cache of the views
        this.collection = new Watches();

        // This is the main list view, which is always around but sometimes
        // hidden. This gets created initially in listWatches.
        this.list_view = null;

        // This is the current view showing
        this.current_view = null;
    },

    listWatches: function() {
        if (this.list_view === null) {
            // Create the view pointing to our collection
            this.list_view = new WatchListView({ collection: this.collection });

            // Bind to the events on the view
            this.list_view.on("newWatch", function() {
                this.navigate("watches/new", { trigger: true });
            }, this);
        }

        // Show it!
        this._setView(this.list_view);
    },

    newWatch: function() {
        // Create the new view
        var view = new WatchNewView({ collection: this.collection });
        view.on("cancel", function() {
            this.navigate("watches", { trigger: true });
        }, this);

        // Swap in our new view
        this._setView(view);

        // Render. Note that this MUST be called after _setView because
        // the ACE editor requires the elements to be visible.
        view.render();
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
    }
});
