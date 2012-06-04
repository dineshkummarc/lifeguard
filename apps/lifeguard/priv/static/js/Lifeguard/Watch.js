var Watch = Backbone.Model.extend({
    idAttribute: "name",
    urlRoot: "/api/watches",

    defaults: {
        "name": "<unknown>",
        "code": "",
        "interval": "",
        "state": "unknown",
        "result": null,
        "timer_at": null
    }
});
