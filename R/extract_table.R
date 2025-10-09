#' Extract a table from a .ice file
#'
#' @param ice_file The file path to the .ice file.
#' @param table_name The name of the table to extract.
#'
#' @details
#' `read_ice()` returns an object of class `"ice"`, which is a named list of
#' tables with metadata stored on the object via the `"metadata"` attribute.
#' This helper retrieves tables directly from that list and relies on the
#' attribute for metadata lookups.
#' @return The requested table as a data frame.
#' @examples
#' \dontrun{
#' extract_table("example.ice", "trade_flows")
#' }
#' @export
extract_table <- function(ice_file, table_name) {
  ice_data <- read_ice(ice_file)
  tables <- as.list(ice_data)

  if (!table_name %in% names(tables)) {
    stop("Table ", table_name, " does not exist in the .ice file.")
  }

  tables[[table_name]]
}
