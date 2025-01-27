#' Create metadata for tables
#'
#' @param tables A named list of data frames to infer metadata from.
#' @param description A general description of the .ice file.
#' @param version A version number for the .ice file (default is "1.0").
#' @param timestamp A timestamp for the .ice file (default is the current time).
#' @return A list containing metadata for the .ice file.
#' @examples
#' tables <- list(
#'   trade_flows = data.frame(country = c("USA", "CHN"), value = c(100, 200)),
#'   tariffs = data.frame(country = c("USA", "CHN"), value = c(0.1, 0.2))
#' )
#' metadata <- create_metadata(tables)
#' @export
create_metadata <- function(tables, description = "", version = "1.0", timestamp = Sys.time()) {
  if (is.null(names(tables)) || any(names(tables) == "")) {
    stop("All elements in 'tables' must be named.")
  }

  metadata <- list(
    description = description,
    version = version,
    timestamp = as.character(timestamp),
    tables = list()
  )

  for (table_name in names(tables)) {
    table <- tables[[table_name]]
    message("Please provide a description for table: ", table_name)
    table_description <- readline("Description: ")

    keys <- lapply(names(table)[-ncol(table)], function(col) {
      list(name = col, type = class(table[[col]])[1], description = paste("Key column for", col))
    })

    value <- list(name = "value", type = class(table[[ncol(table)]])[1], description = "Value column")

    metadata$tables[[table_name]] <- list(
      description = table_description,
      keys = keys,
      value = value
    )
  }

  return(metadata)
}
