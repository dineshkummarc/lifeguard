var WatchDashboardUnitView = Backbone.View.extend({
    tagName: "div",
    template: null,

    initialize: function() {
        if (!WatchDashboardUnitView.template) {
            var templateEl = document.id("watch-dashboard-unit-template");
            var template = _.template(templateEl.get("html"));
            WatchDashboardUnitView.prototype.template = template;
        }

        this.model.on("change", this.render, this);
        this.model.on("destroy", this.remove, this);
    },

    render: function() {
        this.$el.html(this.template(this.model.toJSON()));
        return this;
    }
});
