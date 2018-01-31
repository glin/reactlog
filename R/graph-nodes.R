ContextNode <- R6::R6Class("ContextNode",
  public = list(
    id = NULL,
    label = NULL,
    type = NULL,

    running = FALSE,
    invalidated = FALSE,
    invalidatedBy = NULL,

    prevNode = NULL,
    nextNode = NULL,

    children = list(),
    parents = list(),

    elapsedTime = NULL,
    totalTime = 0,
    execCount = 0,

    timeEnter = NULL,
    timeExit = NULL,

    srcref = NULL,
    srcfile = NULL,

    initialize = function(id, label, type, srcref = NULL, srcfile = NULL) {
      self$id <- id
      self$label <- label
      self$type <- type
      self$srcref <- srcref
      self$srcfile <- srcfile
    },

    format = function(verbose = getOption("reactlog.verbose", FALSE), ...) {
      status <- paste0(self$status, if (self$isInitial) " (initial)")
      invalidatedBy <- self$invalidatedBy
      if (!is.null(invalidatedBy)) {
        invalidatedBy <- invalidatedBy$formatLabel(value = TRUE, changed = TRUE)
      }

      fields <- list(
        id = if (verbose) self$id,
        type = self$type,
        status = status,
        invalidatedBy = invalidatedBy,
        elapsedTime = if (!is.null(self$elapsedTime)) formatSeconds(self$elapsedTime)
      )

      if (verbose) {
        parents <- lapply(self$parents, function(x) x$formatLabel(status = TRUE))

        fields <- c(fields, list(
          dependencies = if (length(self$parents) > 0) formatArray(parents),
          totalTime = formatSeconds(self$totalTime),
          execCount = self$execCount,
          srcref = if (!is.null(self$srcref)) self$formatSrcref()
        ))
      }

      paste0(
        self$formatLabel(), "\n",
        formatDescList(filterNULL(fields))
      )
    },

    formatLabel = function(srcref = FALSE, status = FALSE, maxLines = 6, ...) {
      label <- truncate(self$label, maxLines, placeholder = colorGrey("..."))

      if (status) {
        label <- paste0(label, if (self$invalidated) colorRed("*"))
      }

      if (srcref) {
        srcref <- sprintf(" [%s]", self$formatSrcref())
        label <- paste0(label, colorCyan(srcref))
      }

      label
    },

    formatSrcref = function() {
      sprintf("%s#%s", self$srcfile, self$srcref[1])
    }
  ),

  active = list(
    uid = function() self$id,
    isInitial = function() self$execCount == 0,

    status = function() {
      if (self$running) {
        status <- "running"
      } else if (self$invalidated) {
        status <- "invalidated"
      } else {
        status <- "normal"
      }
      status
    },

    dependencies = function() self$parents,
    dependents = function() self$children
  )
)

ValueNode <- R6::R6Class("ValueNode",
  public = list(
    id = NULL,
    label = NULL,
    value = NULL,
    type = "value",

    changed = FALSE,
    changedBy = NULL,
    changeCount = 0,

    prevNode = NULL,
    nextNode = NULL,

    children = list(),

    initialize = function(id, label = NULL, value = NULL) {
      self$id <- id
      self$label <- if (!is.null(label)) label else id
      self$value <- value
    },

    format = function(verbose = getOption("reactlog.verbose", FALSE), ...) {
      status <- paste0(
        self$status,
        if (self$changed) sprintf(" => %s", self$nextNode$value),
        if (self$isInitial) " (initial)"
      )

      fields <- list(
        id = if (verbose) self$id,
        type = self$type,
        status = status,
        changedBy = self$changedBy %AND% self$changedBy$formatLabel()
      )

      if (verbose) {
        fields <- c(fields, list(
          changeCount = self$changeCount
        ))
      }

      paste0(
        self$formatLabel(value = TRUE), "\n",
        formatDescList(filterNULL(fields))
      )
    },

    formatLabel = function(value = FALSE, changed = FALSE, status = FALSE,
                           maxLines = 6, ...) {
      label <- self$label

      if (status) {
        label <- paste0(label, if (self$changed) colorRed("*"))
      }

      if (value && changed && self$changed) {
        label <- paste(label, colorGrey("=>", self$nextNode$value))
      } else if (value) {
        label <- paste(label, colorGrey("=", self$value))
      }

      truncate(label, maxLines, placeholder = colorGrey("..."))
    }
  ),

  active = list(
    uid = function() sprintf("%s-%s", self$id, self$changeCount),
    isInitial = function() is.null(self$prevNode),

    status = function() {
      if (self$changed) {
        status <- "changed"
      } else {
        status <- "normal"
      }
      status
    },

    dependents = function() self$children
  )
)

is.ContextNode <- function(x) inherits(x, "ContextNode")
is.ValueNode <- function(x) inherits(x, "ValueNode")
is.GraphNode <- function(x) is.ContextNode(x) || is.ValueNode(x)
