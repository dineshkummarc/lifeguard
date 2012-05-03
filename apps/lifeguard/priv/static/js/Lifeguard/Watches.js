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

    delete: function(e) {
        // Use MooTools to stop the default
        (new DOMEvent(e)).preventDefault();

        // Delete it!
        dbg.info("Deleting model: " + this.model.get("name"));
        this.model.destroy();
    },

    render: function() {
        this.$el.html(this.template(this.model.toJSON()));
        return this;
    }
});

var WatchAppView = Backbone.View.extend({
    el: document.id("app-watches"),

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
    }
});
