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
