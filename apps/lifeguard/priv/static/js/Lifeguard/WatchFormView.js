var WatchFormView = Backbone.View.extend({
    tagName: "div",
    template: null,

    events: {
        "click .cancel": "cancel",
        "click .save": "save"
    },

    initialize: function() {
        if (!WatchFormView.template) {
            var templateEl = document.id("watch-new-template");
            var template   = _.template(templateEl.get("html"));;
            WatchFormView.prototype.template = template;

            var errorTemplateEl = document.id("watch-new-errors-template");
            var errorTemplate = _.template(errorTemplateEl.get("html"));
            WatchFormView.prototype.errorTemplate = errorTemplate;
        }

        // If we're given a model, then we're modifying an existing
        // Watch. Otherwise, we're creating a new one.
        if (this.model)
            this.isNew = false;
        else
            this.isNew = true;

        this.rendered = false;
        this.editor = null;
    },

    render: function() {
        if (!this.rendered) {
            // Render and mark as rendered
            this.$el.html(this.template());
            this.rendered = true;

            // Get some of the common elements
            this.save_button = this.el.getElement(".save");
       }

        if (this.editor === null) {
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

        if (!this.isNew) {
            // Update the header to say we're editing
            var header = this.el.getElementById("header");
            header.set("html", "Editing Watch: " + this.model.get("name"));

            // Since we're modifying an existing model, update all
            // the form values with the model data
            var nameField = this.el.getElementById("name");
            nameField.value = this.model.get("name");
            nameField.set("readOnly", true);

            this.el.getElementById("interval").value = this.model.get("interval");
            this.editor.getSession().setValue(this.model.get("code"));
        }

        return this;
    },

    cancel: function(event) {
        event.preventDefault();

        if (window.confirm("Are you sure? Your changes will be lost.")) {
            this.trigger("cancel");
        }
    },

    save: function(event) {
        event.preventDefault();

        // Disable the save button
        this.save_button.addClass("disabled");

        var watch = this.model;
        if (this.isNew)
            watch = new Watch();

        // Set the attributes
        watch.set({
            name: this.el.getElementById("name").value,
            interval: parseInt(this.el.getElementById("interval").value, 10),
            code: this.editor.getSession().getValue()
        });

        // Save it
        watch.save(null, {
            success: _.bind(this.saveSuccess, this),
            error: _.bind(this.saveError, this)
        });
    },

    saveError: function(model, response) {
        // Parse the errors and show the message
        var json      = JSON.parse(response.responseText);
        var errorHtml = this.errorTemplate({ errors: json.errors });
        this.el.getElementById("errors").set("html", errorHtml);

        // Re-enable the save button
        this.save_button.removeClass("disabled");
    },

    saveSuccess: function(model, response) {
        if (this.isNew) {
            // Add the model to our collection
            this.collection.add(model);
        }

        // Trigger an event to get us out of here
        this.trigger("saveComplete");
    }
});
