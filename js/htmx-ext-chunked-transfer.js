// index.ts
(function() {
  let api;
  htmx.defineExtension("chunked-transfer", {
    init: function(apiRef) {
      api = apiRef;
    },
    onEvent: function(name, evt) {
      const elt = evt.target;
      if (name === "htmx:beforeRequest") {
        const xhr = evt.detail.xhr;
        xhr.onprogress = function() {
          const is_chunked = xhr.getResponseHeader("Transfer-Encoding") === "chunked";
          if (!is_chunked)
            return;
          let response = xhr.response;
          api.withExtensions(elt, function(extension) {
            if (!extension.transformResponse)
              return;
            response = extension.transformResponse(response, xhr, elt);
          });
          var swapSpec = api.getSwapSpecification(elt);
          var target = api.getTarget(elt);
          var settleInfo = api.makeSettleInfo(elt);
          api.selectAndSwap(swapSpec.swapStyle, target, elt, response, settleInfo);
          api.settleImmediately(settleInfo.tasks);
        };
      }
    }
  });
})();
