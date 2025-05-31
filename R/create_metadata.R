#' Create metadata for tables
#'
#' @param tables A named list of data frames to infer metadata from.
#' @param description A general description of the .ice file.
#' @param version A version number for the .ice file (default is "1.0").
#' @param timestamp A timestamp for the .ice file (default is the current time).
#' @param non_interactive Logical, if TRUE does not prompt and uses defaults.
#' @param descriptions_file Optional path to a YAML file with table descriptions.
#' @return A list containing metadata for the .ice file.
#' @export
create_metadata <- function(tables, description = "", version = "1.0", timestamp = Sys.time(),
                            non_interactive = FALSE, descriptions_file = NULL) {
  if (is.null(names(tables)) || any(names(tables) == "")) {
    stop("All elements in 'tables' must be named.")
  }

  # Optional description YAML input
  table_descriptions <- list()
  if (!is.null(descriptions_file)) {
    table_descriptions <- yaml::read_yaml(descriptions_file)
  }

  metadata <- list(
    description = description,
    version = version,
    timestamp = as.character(timestamp),
    tables = list()
  )

  for (table_name in names(tables)) {
    table <- tables[[table_name]]
    if (!non_interactive && is.null(table_descriptions[[table_name]])) {
      message("Please provide a description for table: ", table_name)
      cat("Description: ")
      table_description <- readLines("stdin", n = 1)
    } else {
      table_description <- table_descriptions[[table_name]] %||% paste("Table for", table_name)
    }

    keys <- lapply(names(table)[-ncol(table)], function(col) {
      list(name = col, type = class(table[[col]])[1], description = paste("Key column for", col))
    })

    value <- list(name = names(table)[ncol(table)],
                  type = class(table[[ncol(table)]])[1],
                  description = "Value column")

    metadata$tables[[table_name]] <- list(
      description = table_description,
      keys = keys,
      value = value
    )
  }

  return(metadata)
}
