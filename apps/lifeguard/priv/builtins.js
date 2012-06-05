(function() {
    // This is the main lifeguard API that checks call into in order
    // to call back into Erlang.
    var Lifeguard = this.Lifeguard = {
        // This is a result type representing that the watch is in good health.
        HEALTHY: "healthy",

        // This is a result type representing that the watch is in some sort
        // of "warning" state.
        WARNING: "warning",

        // This is a result type representing that the watch is in a critical
        // erroneous state.
        CRITICAL: "critical",

        // This is a result type representing that the watch is in an
        // unknown state.
        UNKNOWN: "unknown",

        // Set the result of a watch. This can be called multiple times if you
        // wish to change the result. The final result at the end of the watch
        // JavaScript execution will be used as the watch result.
        //
        // An optional message may be passed as well, which will be shown
        // with the result.
        //
        // `result` should be one of: Lifeguard.GOOD, Lifeguard.BAD.
        setResult: function(result, message) {
            // Set the result on the _result attribute that is then read
            // later from Erlang.
            Lifeguard._result = {
                result: result,
                message: message
            };
        },

        // Internal method called from Erlang. Do not call this directly.
        _call: function(code) {
            // Clear the previous result
            Lifeguard.setResult(Lifeguard.UNKNOWN, "No result was set.");

            // Call the code in a safer eval with an empty context. One problem
            // here is that the user can still modify the `Lifeguard` object
            // itself but we can solve that later.
            var context = {};
            var wrappedFunc = new Function("Lifeguard", code);
            wrappedFunc.call(context, Lifeguard);

            // Return the result
            return Lifeguard._result;
        }
    };
}).call(this);
