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
