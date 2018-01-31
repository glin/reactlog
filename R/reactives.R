#' Get the currect reactive context
#'
#' @keywords internal
#' @export
getCurrentContext <- function() {
  tryCatch({
    shiny:::getCurrentContext()
  },
  error = function(e) {
    NULL
  })
}

getLastContextId <- function(x) {
  UseMethod("getLastContextId")
}

getLastContextId.Observer <- function(x) {
  x$.prevId
}

getLastContextId.Observable <- function(x) {
  x$.mostRecentCtxId
}

getLastContextId.reactive <- function(x) {
  obs <- attr(x, "observable")
  getLastContextId(obs)
}

getLastContextId.default <- function(x) {
  if (is.Context(x)) {
    return(x$id)
  }
  stop("Expected a reactive context, expression, or observer", call. = FALSE)
}

is.Context <- function(x) {
  # shiny:::Context
  is.environment(x) && is.character(x$id)
}

getOutputObserver <- function(output, name) {
  session <- .subset2(output, "impl")
  outputs <- environment(session$initialize)$private$.outputs
  .subset2(outputs, name)
}

getValueLabel <- function(x, ...) {
  UseMethod("getValueLabel")
}

getValueLabel.default <- function(x, ...) {
  stop("Expected a reactive value or reactive values object", call. = FALSE)
}

getValueLabel.reactiveVal <- function(x, ...) {
  attr(x, "label")
}

getValueLabel.reactivevalues <- function(x, name, ...) {
  label <- .subset2(x, "impl")$.label
  paste0(label, "$", name)
}
