findComponents <- function(graph, state = c("all", "current")) {
  stopifnot(is.ReactGraph(graph))
  state <- match.arg(state)
  visited <- list()
  queue <- Queue$new()

  labels <- list()
  currentLabel <- 0

  for (node in graph$nodes) {
    if (!is.null(labels[[node$uid]])) next

    queue$enqueue(node)
    visited[[node$uid]] <- node

    while (queue$size() > 0) {
      current <- queue$dequeue()

      if (is.null(labels[[current$uid]])) {
        currentLabel <- currentLabel + 1
        labels[[current$uid]] <- currentLabel
      }

      neighbors <- c(current$parents, current$children,
                     if (state == "all") c(current$prevNode, current$nextNode))

      for (neighbor in neighbors) {
        if (is.null(visited[[neighbor$uid]])) {
          queue$enqueue(neighbor)
          visited[[neighbor$uid]] <- neighbor
          labels[[neighbor$uid]] <- labels[[current$uid]]
        }
      }
    }
  }

  idsByComponent <- split(names(labels), as.numeric(labels))
  components <- lapply(idsByComponent, function(ids) visited[ids])
  components
}

getComponentLabels <- function(components) {
  labels <- list()
  for (label in names(components)) {
    for (node in components[[label]]) {
      labels[[node$id]] <- label
    }
  }
  labels
}

findSubcomponent <- function(node,
                             links = c("all", "dependencies", "dependents"),
                             state = c("all", "current")) {
  stopifnot(is.GraphNode(node))
  links <- match.arg(links, several.ok = TRUE)
  state <- match.arg(state)
  visited <- list()
  queue <- Queue$new()

  queue$enqueue(node)
  visited[[node$uid]] <- node

  while (queue$size() > 0) {
    current <- queue$dequeue()

    neighbors <- c(
      if (links == "all" || links == "dependencies") current$parents,
      if (links == "all" || links == "dependents") current$children,
      if (state == "all") c(current$prevNode, current$nextNode)
    )

    for (neighbor in neighbors) {
      if (is.null(visited[[neighbor$uid]])) {
        queue$enqueue(neighbor)
        visited[[neighbor$uid]] <- neighbor
      }
    }
  }

  visited
}
