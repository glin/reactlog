# show the graph for the most recent session
reactlog::showReactGraph()

observe({
  # show the graph filtered on this observer and its dependencies
  reactlog::showReactGraph()
})
