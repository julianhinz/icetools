#' Validate a .ice file
#'
#' @param ice_file The file path to the .ice file.
#' @return A message indicating whether the file is valid.
#' @examples
#' validate_ice("example.ice")
#' @export
validate_ice <- function(ice_file) {
  ice_data <- read_ice(ice_file)
  tables <- ice_data$tables
  metadata <- ice_data$metadata

  for (table_name in names(metadata$tables)) {
    table_meta <- metadata$tables[[table_name]]
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
