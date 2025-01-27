#' Extract a table from a .ice file
#'
#' @param ice_file The file path to the .ice file.
#' @param table_name The name of the table to extract.
#' @return The requested table as a data frame.
#' @examples
#' extract_table("example.ice", "trade_flows")
#' @export
extract_table <- function(ice_file, table_name) {
  ice_data <- read_ice(ice_file)
  tables <- ice_data$tables

  if (!table_name %in% names(tables)) {
    stop("Table ", table_name, " does not exist in the .ice file.")
  }

  tables[[table_name]]
}
