#' Validate a .ice file
#'
#' @param ice_file The file path to the .ice file.
#'
#' @details
#' Assumes that `read_ice()` returns an object of class `"ice"`: a named list
#' of tables with file-level metadata available through the `"metadata"`
#' attribute. The metadata must contain a `tables` entry defining each table's
#' keys and description.
#' @return A message indicating whether the file is valid.
#' @examples
#' \dontrun{
#' validate_ice("example.ice")
#' }
#' @export
validate_ice <- function(ice_file) {
  ice_data <- read_ice(ice_file)
  tables <- as.list(ice_data)
  metadata <- attr(ice_data, "metadata")

  if (is.null(metadata)) {
    stop("Metadata attribute missing from .ice object.")
  }

  table_definitions <- metadata$tables
  if (is.null(table_definitions) || length(table_definitions) == 0) {
    stop("No table definitions found in metadata.")
  }

  for (table_name in names(table_definitions)) {
    table_meta <- table_definitions[[table_name]]
    if (!table_name %in% names(tables)) {
      stop("Table ", table_name, " is missing from the .ice file.")
    }

    table <- tables[[table_name]]

    # Check for NAs
    if (anyNA(table)) {
      stop("Table ", table_name, " contains missing values.")
    }

    # Check keys
    for (key in table_meta$keys) {
      if (!key$name %in% names(table)) {
        stop("Key ", key$name, " is missing from table ", table_name, ".")
      }
    }

    # Check completeness (all combinations of keys must exist)
    key_columns <- sapply(table_meta$keys, function(key) key$name)
    key_combinations <- expand.grid(lapply(table[key_columns], unique))
    if (nrow(key_combinations) != nrow(table)) {
      stop("Table ", table_name, " does not contain all combinations of keys.")
    }

    # Check value
    if (!"value" %in% names(table)) {
      stop("The value column is missing or misnamed in table ", table_name, ".")
    }
  }

  message("Validation successful: The .ice file is properly structured.")
}
