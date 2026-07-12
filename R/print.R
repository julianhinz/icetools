#' Print method for .ice objects
#'
#' @param x The .ice object to print.
#' @param ... Additional arguments (ignored).
#' @export
print.ice <- function(x, ...) {
  if (!inherits(x, "ice")) stop("Object is not of class 'ice'")

  # Retrieve metadata and filename from attributes
  metadata <- attr(x, "metadata")
  filename <- attr(x, "filename")

  cli::cli_h1("File: {.path {basename(filename)}}")

  if (!is.null(metadata)) {
    cli::cli_h2("Metadata")
    cli::cli_text("{.strong Description}: {metadata$description}")
    cli::cli_text("{.strong Timestamp}: {metadata$timestamp}")
    cli::cli_text("{.strong Version}: {metadata$version}")
  } else {
    cli::cli_alert_warning("No metadata found.")
  }

  cli::cli_h2("Tables")
  cli::cli_ul(names(x))
}
