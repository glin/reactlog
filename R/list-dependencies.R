#' List reactive dependencies
#'
#' Shows a tree view of reactive dependencies.
#'
#' @inheritParams getContextNode
#' @param depth Max depth of the dependency tree to display.
#' @param status If `TRUE`, show status markers.
#' @param value If `TRUE`, show values of reactive values.
#' @param srcref If `TRUE`, show source references.
#' @param quiet If `TRUE`, suppress output.
#' @param file A filename or connection to print to. Defaults to `stdout`.
#' @param append If `TRUE`, output will be appended to `file`.
#' @return A nested list of dependencies.
#'
#' @seealso [traceInvalidation()]
#' @export
#'
#' @examples
#' library(shiny)
#' options(shiny.reactlog = TRUE)
#'
#' valA <- reactiveVal(1, label = "valA")
#' valB <- reactiveVal(2, label = "valB")
#'
#' rxA <- reactive({
#'   valA()
#' })
#'
#' rxB <- reactive({
#'   valB()
#' })
#'
#' rxAB <- reactive({
#'   valA() + valB()
#' })
#'
#' obs <- observe({
#'   listDependencies()
#'   rxA() + rxB() + rxAB()
#' }, label = "obs")
#'
#' observe(valA(3))
#'
#' # trigger flush event (happens automatically in a Shiny app)
#' shiny:::flushReact()
#'
#' listDependencies(obs, depth = 0, srcref = TRUE)
#'
#' \dontrun{
#' # show the entire dependency tree
#' listDependencies()
#' }
listDependencies <- function(x = getCurrentContext(), graph = getReactGraph(),
                             invalidated = FALSE, depth = NULL,
                             status = TRUE, value = FALSE, srcref = FALSE,
                             quiet = FALSE, file = "", append = FALSE) {

  if (!is.null(depth) && !is.numeric(depth)) {
    stop("Max depth must be numeric", call. = FALSE)
  }

  treeArgs <- list(maxDepth = depth, status = status, value = value, srcref = srcref)

  if (!is.null(x)) {
    ctx <- getContextNode(x, graph, invalidated = invalidated)
    if (is.null(ctx)) return(invisible())

    if (ctx$status == "running" && !is.null(ctx$prevNode)) {
      # When called within a reactive context, use the previous context since
      # there's no guarantee all dependencies will have been resolved.
      ctx <- ctx$prevNode
    }

    deps <- do.call(buildDependencyTree, c(ctx, treeArgs))
  } else {
    graphDeps <- lapply(graph$nodes, function(node) {
      do.call(buildDependencyTree, c(node, treeArgs))
    })
    deps <- reactDependencyTree(graph$formatLabel(), children = graphDeps)
  }

  if (length(deps$children) == 0) {
    return(invisible())
  }

  if (!quiet) {
    print(deps, file = file, append = append)
  }

  invisible(deps)
}

buildDependencyTree <- function(node, maxDepth = NULL, depth = 0, status = TRUE,
                                value = FALSE, srcref = FALSE) {
  label <- node$formatLabel(status = status, srcref = srcref, value = value)
  deps <- reactDependencyTree(label)

  if (!is.null(maxDepth) && depth > maxDepth) {
    return(deps)
  }

  if (length(node$dependencies) > 0) {
    deps$children <- lapply(
      node$dependencies,
      buildDependencyTree,
      maxDepth = maxDepth,
      depth = depth + 1,
      status = status,
      value = value,
      srcref = srcref
    )
  }

  deps
}

reactDependencyTree <- function(label, children = NULL) {
  deps <- structure(list(label = label), class = "reactDependencyTree")
  if (length(children) > 0) deps$children <- children
  deps
}

#' @export
print.reactDependencyTree <- function(x, ..., file = "", append = FALSE) {
  cat(format(x), "\n", file = file, append = append)
}

#' @export
format.reactDependencyTree <- function(x, ..., root = TRUE, lastChild = TRUE, prefix = "") {
  asciiTree(x)
}

asciiTree <- function(node, root = TRUE, lastChild = TRUE, prefix = "") {
  str <- node$label

  if (!root) {
    line <- paste0(prefix, if (lastChild) "`-- " else "+-- ")
    str <- paste0(colorGrey(line), str)

    prefix <- paste0(prefix, if (lastChild) "  " else "| ")
    str <- prefixNewLines(str, colorGrey(paste0(prefix, "  ")))
  }

  if (length(node$children) > 0) {
    lastChild <- seq_along(node$children) == length(node$children)
    children <- Map(
      asciiTree,
      node$children,
      root = FALSE,
      lastChild = lastChild,
      prefix = prefix
    )
    str <- paste(c(str, children), collapse = "\n")
  }

  str
}

#' @export
as.character.reactDependencyTree <- function(x, ...) {
  format(x, ...)
}
