#' List the components of a .ice file
#'
#' Reads only the archive's `metadata.yaml` (data members are not
#' parsed) and returns one row per component: tables and, for format
#' >= 1.1 archives, scalars.
#'
#' Versions up to 0.1.2 always returned `NULL` here due to a metadata
#' access bug (issue #7).
#'
#' @param ice_file Path to the .ice file.
#' @return A `data.table` with columns `name`, `kind` ("table" or
#'   "scalar"), `description`, `keys` (comma-separated key column names),
#'   `value_name`, and `value_type`.
#' @examples
#' \dontrun{
#' list_tables("example.ice")
#' }
#' @export
list_tables <- function(ice_file) {
  metadata <- read_ice_metadata(ice_file)

  rows <- list()
  for (table_name in names(metadata$tables)) {
    table_meta <- metadata$tables[[table_name]]
    rows[[length(rows) + 1L]] <- data.table::data.table(
      name = table_name,
      kind = "table",
      description = table_meta$description %||% NA_character_,
      keys = paste(vapply(table_meta$keys, function(k) k$name, character(1)),
                   collapse = ", "),
      value_name = table_meta$value$name %||% NA_character_,
      value_type = table_meta$value$type %||% NA_character_
    )
  }
  for (scalar_name in names(metadata$scalars)) {
    scalar_meta <- metadata$scalars[[scalar_name]]
    rows[[length(rows) + 1L]] <- data.table::data.table(
      name = scalar_name,
      kind = "scalar",
      description = scalar_meta$description %||% NA_character_,
      keys = "",
      value_name = scalar_name,
      value_type = scalar_meta$type %||% NA_character_
    )
  }

  if (!length(rows)) {
    return(data.table::data.table(
      name = character(0), kind = character(0), description = character(0),
      keys = character(0), value_name = character(0), value_type = character(0)
    ))
  }
  data.table::rbindlist(rows)
}

# Extract and parse only metadata.yaml from a .ice archive.
read_ice_metadata <- function(ice_file) {
  if (!file.exists(ice_file)) {
    cli::cli_abort("File does not exist: {.path {ice_file}}")
  }
  temp_dir <- tempfile("ice_metadata")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
  ok <- tryCatch(
    { utils::untar(ice_file, files = "metadata.yaml", exdir = temp_dir, tar = "internal"); TRUE },
    error = function(e) FALSE, warning = function(w) FALSE
  )
  meta_path <- file.path(temp_dir, "metadata.yaml")
  if (!ok || !file.exists(meta_path)) {
    cli::cli_abort("{.path {ice_file}} is not a valid .ice archive: no {.file metadata.yaml} member.")
  }
  yaml::read_yaml(meta_path)
}
