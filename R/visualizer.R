#' Reactive log visualizer
#'
#' Launches the reactive log visualizer with extra features:
#' - Interactive filtering on selected nodes
#' - Jump to the previous/next flush cycle (i.e. all white nodes)
#'
#' @param time If `TRUE`, display the time for each reactive.
#' @param filter Filter on a reactive observer or expression and its dependencies.
#'   Defaults to the current reactive context if present.
#' @param graph A reactive graph. Defaults to the reactive graph for the
#'   current Shiny session.
#'
#' @note Experimental. Current limitations include:
#'   - Can only be launched from R like [shiny::showReactLog()]
#'   - Interactive filtering limited to all connected nodes (both dependencies
#'       and dependents)
#'   - Unable to tell when an observer is destroyed (stays grey forever)
#'
#' @seealso [shiny::showReactLog()]
#' @export
#'
#' @examples
#' \dontrun{
#' # show the graph for most recent session
#' showReactGraph()
#'
#' observe({
#'   # show the graph filtered on this observer and its dependencies
#'   showReactGraph()
#' })
#' }
showReactGraph <- function(graph = getReactGraph(), time = TRUE,
                           filter = getCurrentContext()) {
  file <- renderReactGraph(graph = graph, time = time, filter = filter)
  utils::browseURL(file)
}

#' @export
reactGraphViewer <- function(session = shiny::getDefaultReactiveDomain(),
                             time = TRUE, filter = getCurrentContext()) {
  if (!reactLogEnabled()) return(NULL)

  url <- session$registerDataObj("reactgraph", NULL, function(data, req) {
    file <- renderReactGraph(time = time, filter = filter)
    shiny:::httpResponse(content = list(file = file, owned = TRUE))
  })

  session$sendCustomMessage("reactgraph", list(url = url))
}

#' @export
reactlogLib <- function() {
  if (!reactLogEnabled()) return(NULL)

  htmltools::htmlDependency(
    "reactlog",
    utils::packageVersion("reactlog"),
    system.file("www", package = "reactlog"),
    script = "reactlog.js"
  )
}

renderReactGraph <- function(graph = getReactGraph(), time = TRUE,
                             filter = getCurrentContext()) {
  sessionId <- graph$sessionId
  node <- getContextNode(filter, graph = graph)

  if (!is.null(node)) {
    nodes <- findSubcomponent(node, links = "dependencies")
    nodeIds <- unique(vapply(nodes, function(x) x$id, character(1)))
  } else {
    nodeIds <- NULL
  }

  log <- filterReactLog(
    shiny:::.graphStack$as_list(),
    sessionId = sessionId,
    nodeIds = nodeIds
  )

  components <- findComponents(graph)
  componentLabels <- getComponentLabels(components)

  file <- renderReactLog(log, componentLabels, time = as.logical(time))
  file
}

filterReactLog <- function(log, sessionId = NULL, nodeIds = NULL) {
  filterEvent <- function(e) {
    if (!identical(e$session, sessionId)) {
      return(FALSE)
    }

    if (!is.null(nodeIds) && !e$id %in% nodeIds) {
      return(FALSE)
    }

    TRUE
  }

  Filter(filterEvent, log)
}

renderReactLog <- function(log, componentLabels, time = TRUE) {
  templateFile <- system.file("www/reactive-graph.html", package = "shiny")
  html <- paste(readLines(templateFile), collapse = "\n")

  scriptFile <- system.file("www/reactive-graph.js", package = "reactlog")
  script <- paste(readLines(scriptFile), collapse = "\n")
  script <- sub("__COMPONENT_LABELS__", toJSON(componentLabels), script)

  data <- paste0(toJSON(log), "\n", script)

  html <- sub("__DATA__", data, html, fixed = TRUE)
  html <- sub("__TIME__", paste0('"', time, '"'), html, fixed = TRUE)

  file <- tempfile(fileext = ".html")
  writeLines(html, file)
  return(file)
}
