#' Read an .ice file
#'
#' This function reads an .ice file, extracts its contents, and loads the tables as `data.table` objects.
#' The metadata is stored as an attribute of the returned object.
#'
#' @param ice_file The file path to the .ice file.
#' @return A list containing the extracted tables as `data.table` objects, with metadata stored as an attribute.
#' @examples
#' \dontrun{
#' ice <- read_ice("example.ice")
#' print(ice)
#' data.table::is.data.table(ice$trade_flows)  # Should return TRUE
#' }
#' @export
read_ice <- function(ice_file) {
  # Unpack .ice file
  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::untar(ice_file, exdir = temp_dir)

  # Read metadata
  metadata <- yaml::read_yaml(file.path(temp_dir, "metadata.yaml"))

  # Read tables and ensure they are data.tables
  tables <- list()
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  for (csv_file in csv_files) {
    table_name <- tools::file_path_sans_ext(basename(csv_file))
    table <- data.table::fread(csv_file)  # Read as data.table
    # numeric round-trip guarantee: a `value` column is ALWAYS double -
    # fread's type inference parses integral-looking columns as integer,
    # which downstream consumers must not have to defend against
    if ("value" %in% names(table))
      data.table::set(table, j = "value", value = as.numeric(table[["value"]]))
    if (!is.null(metadata$tables[[table_name]])) {
      attr(table, "metadata") <- metadata$tables[[table_name]]
    }
    tables[[table_name]] <- table
  }

  # Set metadata and filename as attributes of the list
  attr(tables, "metadata") <- metadata
  attr(tables, "filename") <- ice_file

  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)

  # Assign class
  class(tables) <- "ice"

  return(tables)
}
