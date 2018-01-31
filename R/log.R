#' Get the reactive log
#'
#' @return A data frame of logged reactive events.
#'
#' @export
getReactLog <- function() {
  fieldNames <- c(
    "time", "session", "action", "id", "value",
    "label", "type", "prevId", "dependsOn"
  )
  log <- shiny:::.graphStack$as_list()
  log <- lapply(log, `[` , fieldNames)
  log <- as.data.frame(do.call(rbind, log))
  if (nrow(log) == 0) return(log)
  is.na(log) <- log == "NULL"
  colnames(log) <- fieldNames
  for (fieldName in fieldNames) {
    log[[fieldName]] <- unlist(log[[fieldName]])
  }
  log
}

#' Clear the reactive log
#'
#' Convenient way to clear the reactive log without having to start a new
#' R session or unload Shiny. Normally, the reactive log only grows in size
#' and never shrinks.
#'
#' @note Also clears the reactive graph.
#'
#' @export
clearReactLog <- function() {
  log <- shiny:::.graphStack
  log$initialize()
  log$private$count <- 0
  clearReactGraph()
}

#' Enable or disable reactive event logging
#'
#' Convenience functions for `options(shiny.reactlog = TRUE)` and
#' `options(shiny.reactlog = NULL)`
#'
#' @export
enableReactLog <- function() {
  options(shiny.reactlog = TRUE)
}

#' @rdname enableReactLog
#' @export
disableReactLog <- function() {
  options(shiny.reactlog = NULL)
}

reactLogEnabled <- function() {
  isTRUE(getOption("shiny.reactlog"))
}
