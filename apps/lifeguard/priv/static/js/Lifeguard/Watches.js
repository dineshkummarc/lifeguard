var Watches = Backbone.Collection.extend({
    model: Watch,
    url: "/api/watches",

    parse: function(response) {
        return response.watches;
    }
});
