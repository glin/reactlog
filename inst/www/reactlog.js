(function () {
  Shiny.addCustomMessageHandler('reactgraph', function (message) {
    $(document).on('keydown', function (e) {
      if (e.which !== 113 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
        return;
      openReactGraph(message.url);
      e.preventDefault();
    });
  });

  function openReactGraph(url) {
    var viewerWindow = window.open(url);

    Shiny.addCustomMessageHandler('reactlog', function (message) {
      viewerWindow.postMessage({ reactlog: message }, '*');
    });
  }
})();