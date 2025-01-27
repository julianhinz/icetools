#' Print method for .ice objects
#'
#' @param x The .ice object to print.
#' @param ... Additional arguments (ignored).
#' @export
print.ice <- function(x, ...) {
  cat("Initial Conditions Metadata:\n")
  cat("Description: ", x$metadata$description, "\n", sep = "")
  cat("Timestamp: ", x$metadata$timestamp, "\n", sep = "")
  cat("Version: ", x$metadata$version, "\n", sep = "")
  cat("\nTables:\n")
  print(names(x$tables))
}
