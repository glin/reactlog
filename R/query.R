#' Get a reactive context node
#'
#' @param x A reactive context, expression, observer. Defaults to the
#'   current reactive context.
#' @param graph A reactive graph. Defaults to the reactive graph for the
#'   current Shiny session.
#' @param invalidated If `TRUE`, get the last invalidated context.
#' @return A reactive context node.
#'
#' @family graph queries
#'
#' @export
getContextNode <- function(x = getCurrentContext(), graph = getReactGraph(),
                           invalidated = FALSE) {
  stopifnot(is.ReactGraph(graph))

  if (is.null(x)) return(NULL)

  if (is.ContextNode(x)) {
    ctx <- x
  } else {
    ctxId <- if (is.character(x)) x else getLastContextId(x)
    ctx <- graph$nodes[[ctxId]]
  }

  if (!is.ContextNode(ctx)) {
    if (shiny::is.reactive(x) && ctxId == "") {
      # Unevaluated reactive expression
      return(NULL)
    }

    msg <- sprintf('Could not find a context with ID "%s" in the reactive graph', ctxId)

    if (!reactLogEnabled()) {
      msg <- paste0(
        msg, "\n",
        "Did you enable the reactive log? See ?getReactGraph")
    }

    stop(msg, call. = FALSE)
  }

  if (invalidated && !ctx$invalidated) {
    ctx <- ctx$prevNode
  }

  ctx
}

#' Get a reactive value node
#'
#' @param x A reactive value or reactive values object.
#' @param name The name of a value in a reactive values object.
#' @param graph A reactive graph. Defaults to the reactive graph for the
#'   current Shiny session.
#' @return A reactive value node.
#'
#' @family graph queries
#'
#' @export
getValueNode <- function(x, name = NULL, graph = getReactGraph()) {
  stopifnot(is.ReactGraph(graph))

  if (is.ValueNode(x)) return(x)

  if (shiny::is.reactivevalues(x) && is.null(name)) {
    stop("The name of a reactive value must be specified", call. = FALSE)
  }

  label <- if (is.character(x)) x else getValueLabel(x, name = name)
  value <- graph$nodes[[label]]

  if (!is.ValueNode(value)) {
    msg <- sprintf(
      'Could not find a value with label "%s" in the reactive graph',
      label
    )

    if (!reactLogEnabled()) {
      msg <- paste0(
        msg, "\n",
        "Did you enable the reactive log? See ?getReactGraph"
      )
    }

    stop(msg, call. = FALSE)
  }

  value
}
