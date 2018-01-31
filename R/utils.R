`%OR%` <- function(x, y) if (!is.null(x)) x else y
`%AND%` <- function(x, y) if (is.null(x)) x else y

filterNULL <- function(x) x[!vapply(x, is.null, logical(1))]

formatSeconds <- function(seconds) {
  ifelse(seconds < 1, sprintf("%.0f ms", seconds * 1000), sprintf("%.1f s", seconds))
}

formatArray <- function(x) {
  paste0("[", paste(x, collapse = ", "), "]")
}

formatDescList <- function(x) {
  names <- format(paste0(names(x), "  "))
  width <- max(nchar(names))
  values <- lapply(x, function(value) {
    value <- format(value)
    lines <- unlist(strsplit(value, "\n"))
    paste(c(lines[1], indent(lines[-1], width)), collapse = "\n")
  })
  paste0(names, values, collapse = "\n")
}

strrep <- function(x, times) {
  paste(rep(x, times), collapse = "")
}

indent <- function(str, size) {
  gsub("(?m)(^)", paste0("\\1", strrep(" ", size)), str, perl = TRUE)
}

indentNewLines <- function(str, size) {
  gsub("(\n)", paste0("\\1", strrep(" ", size)), str, perl = TRUE)
}

prefixNewLines <- function(str, prefix) {
  gsub("(\n)", paste0("\\1", prefix), str, perl = TRUE)
}

truncate <- function(str, maxLines, placeholder = colorGrey("...")) {
  lines <- unlist(strsplit(str, "\n"))
  if (length(lines) <= maxLines) return(str)
  paste(c(utils::head(lines, maxLines), placeholder), collapse = "\n")
}

toJSON <- function(x, auto_unbox = TRUE, ...) {
  jsonlite::toJSON(x, auto_unbox = auto_unbox, ...)
}

condition <- function(class, message, call = sys.call(-1), ...) {
  structure(
    list(message = message, call = call),
    class = c(class, "condition"),
    ...
  )
}

colorGrey <- function(...) crayon::make_style("#888888", grey = TRUE)(...)
colorRed <- crayon::red
colorCyan <- crayon::cyan
