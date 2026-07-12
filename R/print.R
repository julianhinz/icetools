#' Print method for ice objects
#'
#' @param x An object of class `"ice"` as returned by [read_ice()].
#' @param ... Additional arguments (ignored).
#' @return `x`, invisibly.
#' @export
print.ice <- function(x, ...) {
  metadata <- attr(x, "metadata")
  filename <- attr(x, "filename")

  cli::cli_h1("File: {.path {basename(filename %||% '<in memory>')}}")

  if (!is.null(metadata)) {
    cli::cli_h2("Metadata")
    cli::cli_text("{.strong Description}: {metadata$description}")
    cli::cli_text("{.strong Timestamp}: {metadata$timestamp}")
    cli::cli_text("{.strong Version}: {metadata$version}")
    if (!is.null(metadata$format_version)) {
      cli::cli_text("{.strong Format}: {metadata$format_version}")
    }
  } else {
    cli::cli_alert_warning("No metadata found.")
  }

  is_table <- vapply(x, is.data.frame, logical(1))
  if (any(is_table)) {
    cli::cli_h2("Tables")
    dims <- vapply(names(x)[is_table], function(nm) {
      paste0(nm, " (", nrow(x[[nm]]), " × ", ncol(x[[nm]]), ")")
    }, character(1))
    cli::cli_ul(dims)
  }
  if (any(!is_table)) {
    cli::cli_h2("Scalars")
    vals <- vapply(names(x)[!is_table], function(nm) {
      paste0(nm, " = ", format(x[[nm]]))
    }, character(1))
    cli::cli_ul(vals)
  }

  invisible(x)
}
