#' List tables in a .ice file
#'
#' @param ice_file The file path to the .ice file.
#'
#' @details
#' Expects the object returned by `read_ice()` to be of class `"ice"`, which
#' stores tables as elements of the list and table metadata in the `"metadata"`
#' attribute. The metadata must contain a `tables` entry describing each table.
#' @return A data frame with table names and descriptions.
#' @examples
#' \dontrun{
#' list_tables("example.ice")
#' }
#' @export
list_tables <- function(ice_file) {
  ice_data <- read_ice(ice_file)
  metadata <- attr(ice_data, "metadata")

  if (is.null(metadata)) {
    stop("Metadata attribute missing from .ice object.")
  }

  if (is.null(metadata$tables) || length(metadata$tables) == 0) {
    return(data.frame(name = character(), description = character(), stringsAsFactors = FALSE))
  }

  table_list <- Map(
    function(table, name) {
      description <- if (!is.null(table$description)) table$description else ""
      data.frame(name = name, description = description, stringsAsFactors = FALSE)
    },
    metadata$tables,
    names(metadata$tables)
  )

  do.call(rbind, table_list)
}
