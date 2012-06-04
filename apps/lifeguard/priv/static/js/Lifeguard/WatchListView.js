var WatchListView = Backbone.View.extend({
    el: document.id("watches-list"),

    events: {
        "click .new":  "newWatch"
    },

    initialize: function() {
        // Get handles to elements we use a lot
        var table       = this.el.getElement("table.watches");
        this.tbody      = table.getElement("tbody");
        this.no_watches = this.el.getElement(".no-watches");

        // Make the table sortable
        var htmlTable = new HtmlTable(table);
        htmlTable.enableSort();
        htmlTable.sort(0, false);

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
            this.no_watches.setStyle("display", "table-row");
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
        el.inject(this.tbody);
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
