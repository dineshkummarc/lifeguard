var WatchDashboardView = Backbone.View.extend({
    el: document.id("watches-dashboard"),

    initialize: function() {
        // Hook up to the collection events so we know when to render
        this.collection.on("add", this.addOne, this);
        this.collection.on("reset", this.addAll, this);
        this.collection.on("all", this.render, this);

        // Kick off the fetch to grab all our watches
        this.collection.fetch();
    },

    render: function() {},

    addAll: function() {
        dbg.debug("Adding all elements from the watches collection");
        this.collection.each(this.addOne, this);
    },

    addOne: function(watch) {
        // Create the view
        var view = new WatchDashboardUnitView({ model: watch });
        var el = view.render().el;
        el.inject(this.el);
    }
});
