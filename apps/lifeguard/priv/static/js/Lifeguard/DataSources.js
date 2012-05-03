var DataSource = Backbone.Model.extend({
    idAttribute: "name",

    defaults: {
        "name": "<UNKNOWN>"
    },

    /**
     * Gets data for this data source from the server.
     * */
    getData: function(options) {
        // Default options
        options = _.defaults(options, {
            args: [],
            onSuccess: function(x) {},
            onFailure: function(x) {}
        });

        // Make the request to the server
        args = JSON.stringify(options.args);
        dbg.info("Requesting data for " + this.get("name") + ": " + args);
        new Request.JSON({
            method: "get",
            url: this.url(),
            onSuccess: options.onSuccess,
            onFailure: options.onFailure
        }).get({ "args": args });
    }
});

var DataSources = Backbone.Collection.extend({
    model: DataSource,
    url: "/api/data-sources",

    parse: function(response) {
        // The API returns an array of data source names currently, so
        // we need to transform those into actual data source objects.
        return _.map(response, function(name) {
            return { "name": name };
        });
    }
});

var DataSourceView = Backbone.View.extend({
    tagName: "li",
    template: null,

    events: {
        "click .name": "toggleConsole",
        "submit .console form": "testGet"
    },

    initialize: function() {
        if (!DataSourceView.template) {
            var templateEl = document.id("data-source-template");
            var template   = _.template(templateEl.get("html"));
            DataSourceView.prototype.template = template;
        }

        this.model.on("change", this.render, this);
        this.model.on("destroy", this.remove, this);
    },

    render: function() {
        this.$el.html(this.template(this.model.toJSON()));
        this.$el[0].getElement(".console").setStyle("display", "none");
        return this;
    },

    testGet: function(event) {
        event.preventDefault();

        // Get the arguments
        var console = this.$el[0].getElement(".console");
        var args = console.getElement("input[name=args]").value;
        if (args !== "")
            args = JSON.parse(args);

        // The results container
        var pre = this.$el[0].getElement("pre.results");

        // Make the actual request
        this.model.getData({
            args: args,
            onSuccess: function(data) {
                // Convert the data to JSON again so we can pretty-print it
                var pretty = JSON.stringify(data, undefined, 2);

                // Put the result into the results pre
                pre.set("html", pretty);
            }
        });
    },

    toggleConsole: function() {
        var console = this.$el[0].getElement(".console");

        if (console.getStyle("display") == "none") {
            console.setStyle("display", "block");
        } else {
            console.setStyle("display", "none");
        }
    }
});

var DataSourcesView = Backbone.View.extend({
    // This is the main application view, so it attaches to a previously
    // existing element.
    el: document.id("data-sources"),

    initialize: function() {
        this.list    = this.$("#data-sources-list")[0];
        this.loading = this.$("#data-sources-loading")[0];

        this.collection.on("reset", this.addAll, this);
        this.collection.on("all", this.render, this);

        // Kick off the fetch to grab all our data sources
        this.collection.fetch();
    },

    render: function() {
        // If we have any items in our collection then we hide the loading
        // dialog
        if (this.collection.length) {
            dbg.info("Collection has elements, hiding loading indicator.");
            this.loading.setStyle("display", "none");
        }

        return this;
    },

    addAll: function() {
        dbg.debug("Adding all elements from the collection");
        this.collection.each(this.addOne, this);
    },

    addOne: function(ds) {
        var view = new DataSourceView({ model: ds });
        var el   = view.render().el;
        el.inject(this.list);
    }
});
