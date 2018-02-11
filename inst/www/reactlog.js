(function () {
  Shiny.addCustomMessageHandler('reactgraph', function (message) {
    $(document).on('keydown', function (e) {
      if (e.which !== 113 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
        return;
      window.open(message.url);
      e.preventDefault();
    });
  });
})();