#' Get the reactive graph
#'
#' Builds a reactive dependency graph from the reactive log.
#'
#' Reactive event logging must be enabled to build graphs:
#' `options(shiny.reactlog = TRUE)` or [enableReactLog()]
#'
#' To clear the reactive graph, start a new R session or call [clearReactLog()].
#'
#' @param session A Shiny session object or session ID. Defaults to the current
#'   session. If `NULL` or no current session, the most recently active session
#'   is used.
#' @return A reactive graph object for the Shiny session.
#'
#' @note A reactive graph object is created when `getReactGraph()` is first
#' called for a Shiny user session. On subsequent calls to `getReactGraph()`,
#' the same graph is retrieved and automatically updated rather than being
#' completely rebuilt.
#
#' @export
getReactGraph <- function(session = shiny::getDefaultReactiveDomain()) {
  sessionId <- if (is.character(session)) session else session$token
  graph <- .getGraph(sessionId = sessionId)

  if (!is.ReactGraph(graph)) {
    msg <- sprintf('Could not find a reactive graph for a session with ID "%s"', sessionId)

    if (!reactLogEnabled()) {
      msg <- paste0(
        msg, "\n",
        "Did you enable the reactive log? See ?getReactGraph")
    }

    stop(msg, call. = FALSE)
  }

  graph
}

clearReactGraph <- function() {
  .initGraph()
}

ReactGraph <- R6::R6Class("ReactGraph",
  public = list(
    nodes = list(),
    sessionId = NULL,
    .ctxStack = NULL,

    initialize = function(sessionId = NULL) {
      self$sessionId <- sessionId
      self$.ctxStack <- Stack$new()
    },

    formatLabel = function() {
      if (!is.null(self$sessionId)) {
        label <- sprintf("sessionId: %s", self$sessionId)
      } else {
        label <- "(no session)"
      }
      label
    }
  )
)

is.ReactGraph <- function(x) inherits(x, "ReactGraph")

addGraphEvent <- function(graph, event) {
  eventProcessors[[event$action]](graph, event)
  invisible(graph)
}

eventProcessors <- list(
  ctx = function(graph, e) {
    node <- ContextNode$new(
      id = e$id,
      label = e$label,
      type = e$type,
      srcref = e$srcref,
      srcfile = e$srcfile
    )

    prev <- graph$nodes[[e$prevId]]

    if (!is.null(prev)) {
      graph$nodes[[prev$id]] <- NULL
      prev$nextNode <- node
      node$prevNode <- prev
      node$totalTime <- prev$totalTime
      node$execCount <- prev$execCount
    }

    graph$nodes[[node$id]] <- node
  },

  enter = function(graph, e) {
    ctx <- graph$nodes[[e$id]]

    ctx$running <- TRUE
    ctx$execCount <- ctx$execCount + 1
    ctx$timeEnter <- e$time

    ctx$callingCtx <- graph$.ctxStack$peek()

    graph$.ctxStack$push(ctx)
  },

  exit = function(graph, e) {
    ctx <- graph$nodes[[e$id]]

    if (!is.ContextNode(ctx)) {
      cond <- condition(
        "contextNotFound",
        sprintf('Could not find a context with ID "%s" in the reactive log', e$id)
      )
      stop(cond)
    }

    ctx$running <- FALSE
    ctx$timeExit <- e$time
    ctx$elapsedTime <- ctx$timeExit - ctx$timeEnter
    ctx$totalTime <- ctx$totalTime + ctx$elapsedTime

    graph$.ctxStack$pop()
  },

  dep = function(graph, e) {
    childCtx <- graph$nodes[[e$id]]
    parentVal <- graph$nodes[[e$dependsOn]]

    if (is.null(parentVal)) {
      parentVal <- ValueNode$new(id = e$dependsOn)
      graph$nodes[[parentVal$id]] <- parentVal
    }

    childCtx$parents[[parentVal$id]] <- parentVal
    parentVal$children[[childCtx$id]] <- childCtx
  },

  depId = function(graph, e) {
    childCtx <- graph$nodes[[e$id]]
    parentCtx <- graph$nodes[[e$dependsOn]]

    childCtx$parents[[parentCtx$id]] <- parentCtx
    parentCtx$children[[childCtx$id]] <- childCtx
  },

  invalidate = function(graph, e) {
    ctx <- graph$nodes[[e$id]]
    ctx$invalidated <- TRUE

    for (node in ctx$parents) {
      if (node$invalidated %OR% node$changed) {
        ctx$invalidatedBy <- node
        break
      }
    }

    if (is.null(ctx$invalidatedBy)) {
      ctx$invalidatedBy <- graph$.ctxStack$peek()
    }
  },

  valueChange = function(graph, e) {
    value <- ValueNode$new(id = e$id, value = e$value)
    prev <- graph$nodes[[value$id]]

    if (!is.null(prev)) {
      prev$changed <- TRUE
      prev$changedBy <- graph$.ctxStack$peek()
      prev$nextNode <- value
      value$prevNode <- prev
      value$changeCount <- prev$changeCount + 1
    }

    graph$nodes[[value$id]] <- value
  }
)

.graph <- new.env(parent = emptyenv())

.getGraph <- function(sessionId = NULL) {
  if (is.null(.graph$size)) .initGraph()

  .updateGraph()

  if (!is.null(sessionId)) {
    graph <- .graph$sessions[[sessionId]]
  } else {
    graph <- .graph$mostRecent
  }
  graph
}

.initGraph <- function() {
  .graph$default <- ReactGraph$new(sessionId = NULL)
  .graph$sessions <- list()
  .graph$mostRecent <- .graph$default
  .graph$size <- 0
}

.updateGraph <- function() {
  size <- shiny:::.graphStack$size()
  if (is.null(.graph$size) || .graph$size > size) .initGraph()
  if (.graph$size == size) return()

  log <- shiny:::.graphStack$as_list()
  newEvents <- log[(.graph$size + 1):size]

  for (event in newEvents) {
    sessionId <- event$session

    if (is.null(sessionId)) {
      graph <- .graph$default
    } else {
      graph <- .graph$sessions[[sessionId]]
    }

    if (is.null(graph)) {
      graph <- ReactGraph$new(sessionId = sessionId)
      .graph$sessions[[sessionId]] <- graph
    }

    graph <- tryCatch({
      addGraphEvent(graph, event)
    },
    contextNotFound = function(cond) {
      # Context$run exits the reactive domain *before* logging the context exit,
      # occasionally causing context exits to be logged with the wrong session.
      # In case this happens, check if another session owns the context.
      for (graph in .graph$sessions) {
        if (is.ContextNode(graph$nodes[[event$id]])) {
          return(addGraphEvent(graph, event))
        }
      }
      stop(cond)
    })
  }

  .graph$mostRecent <- graph
  .graph$size <- size
}
