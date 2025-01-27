#' List tables in a .ice file
#'
#' @param ice_file The file path to the .ice file.
#' @return A data frame with table names and descriptions.
#' @examples
#' list_tables("example.ice")
#' @export
list_tables <- function(ice_file) {
  ice_data <- read_ice(ice_file)
  metadata <- ice_data$metadata

  table_list <- lapply(metadata$tables, function(table, name) {
    data.frame(name = name, description = table$description)
  }, names(metadata$tables))

  do.call(rbind, table_list)
}
