#' Print the call stack that caused invalidation
#'
#' Prints the call stack that caused a reactive context to be invalidated.
#' The most recent call is printed first.
#'
#' @inheritParams getContextNode
#' @param n Number of invalidations to trace back.
#'   Defaults to 1, the most recent invalidation.
#' @param value If `TRUE`, show changed values of reactive values.
#' @param quiet If `TRUE`, suppress output.
#' @param file A filename or connection to print to. Defaults to `stdout`.
#' @param append If `TRUE`, output will be appended to `file`.
#' @return A list (stack) of reactive graph nodes.
#'
#' @seealso [listDependencies()]
#' @export
#'
#' @examples
#' library(shiny)
#' options(shiny.reactlog = TRUE)
#'
#' val <- reactiveVal(1, label = "val")
#'
#' rx <- reactive({
#'   val()
#' })
#'
#' observe({
#'   traceInvalidation()
#'   rx()
#' })
#'
#' observe(val(10))
#'
#' # trigger flush event (happens automatically in a Shiny app)
#' shiny:::flushReact()
#'
#' traceInvalidation(rx)
traceInvalidation <- function(x = getCurrentContext(), graph = getReactGraph(),
                              n = 1, value = TRUE, quiet = FALSE,
                              file = "", append = FALSE) {

  if (!is.numeric(n)) {
    stop("`n` must be numeric", call. = FALSE)
  }

  stack <- .traceInvalidation(x, graph = graph, n = n)

  if (is.null(stack) || stack[[1]]$isInitial) {
    return(invisible())
  }

  if (!quiet) {
    print(stack, value = value, file = file, append = append)
  }

  invisible(stack)
}

.traceInvalidation <- function(x = getCurrentContext(), graph = getReactGraph(),
                               n = 1) {

  node <- getContextNode(x, graph, invalidated = TRUE)

  while (n > 1) {
    node <- node$prevNode
    n <- n - 1
  }

  if (is.null(node)) return(NULL)

  stack <- list(node)

  while (!is.null(node$invalidatedBy)) {
    node <- node$invalidatedBy
    stack[[length(stack) + 1]] <- node
  }

  if (is.ValueNode(node) && node$changed) {
    callingCtx <- node$changedBy$prevNode %OR% node$changedBy
    stack <- c(stack, .traceInvalidation(callingCtx, graph = graph))
  }

  structure(stack, class = "reactStackTrace")
}

#' @export
print.reactStackTrace <- function(x, ..., file = "", append = FALSE) {
  cat(format(x, ...), "\n", file = file, append = append)
}

#' @export
format.reactStackTrace <- function(x, ..., value = TRUE) {
  indexLabels <- sprintf("%s: ", rev(seq_len(length(x))))

  nodeLabels <- lapply(x, function(node) {
    node$formatLabel(value = value, changed = TRUE, srcref = TRUE)
  })

  width <- max(nchar(indexLabels))
  indexLabels <- formatC(indexLabels, width = width)
  nodeLabels <- indentNewLines(nodeLabels, width)
  labels <- paste0(indexLabels, nodeLabels)
  paste(labels, collapse = "\n")
}

#' @export
as.character.reactStackTrace <- function(x, ...) {
  format(x, ...)
}
